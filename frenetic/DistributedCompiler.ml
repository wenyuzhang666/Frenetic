open Core.Std
open Async.Std
open Async_parallel.Std

module Types = NetKAT_Types

let buffer_age_limit = `Unlimited

let p s = Printf.printf "%s:%s: %s\n%!" (Unix.gethostname ())
  (Pid.to_string (Unix.getpid ())) s

exception Remote_exception of string * string

let heap_stats () = 
  let open Core.Std.Gc in
  let n = allocated_bytes () /. 1024. /. 1024. in
  sprintf "heap size = %f MB" n

let measure_time label (f : unit -> 'a Deferred.t) : 'a Deferred.t =
  let start = Time.now () in
  f ()
  >>= fun r ->
  let end_ = Time.now () in
  let span = Time.diff end_ start  in
  let s = heap_stats () in
  p (sprintf "%s took %s; %s" label (Time.Span.to_string span) s);
  return r

let cluster_map (lst : 'a list) 
               ~(workers : string list) 
               ~(f : 'a -> 'b Deferred.t) : ('a, exn) Result.t list Deferred.t =
  let (workers_r, workers_w) = Pipe.create () in
  List.iter workers ~f:(Pipe.write_without_pushback workers_w);
  let run_on_worker x =
    Pipe.read workers_r
    >>= function
    | `Eof -> failwith "unexpected EOF from cluster_map worker pipe"
    | `Ok worker -> 
       try_with
         (fun () -> Parallel.run ~buffer_age_limit ~where:(`On worker) (fun () -> f x))
       >>= (function
       | Error exn -> 
         p (sprintf "Exception from %s: %s" worker (Exn.to_string exn));
         return (Error exn)
       | Ok (Ok y) -> 
         return (Ok y)
       | Ok (Error str) -> 
         p (sprintf "Remote exception from %s: %s" worker str);       
         return (Error (Remote_exception (str, worker))))
       >>= fun r ->
       Pipe.write_without_pushback workers_w worker;
       return r in
  Deferred.List.map ~how:`Parallel lst ~f:run_on_worker

let rec range (min : int) (max : int) : int list =
  if min = max then [max] else min :: range (min + 1) max

let make_scheduler (workers : string list) =
  let workers = Array.of_list workers in
  let i = ref 0 in
  let next () =
    let r = workers.(!i) in
    i := if !i = Array.length workers - 1 then 0 else !i + 1;
    r in
  `F next

(* 

 An example configuration:

{
  workers: [ "10.152.136.136" ]

}

*)
type config = {
  workers: string list;   (* hostnames or IP addresses of worker machines  *)
}


let parse_config (filename : string) : config = 
  let json = Yojson.Basic.from_file filename in
  let open Yojson.Basic.Util in
  let workers = json |> member "workers" |> to_list |> filter_string in
  { workers }

(* Parses src and caches it to dump. It is up to you to "clear the cache"
   by deleting dump if you change src. *)
let parse_caching (src : string) (dump : string) : Types.policy Deferred.t =
  Sys.is_file dump
  >>= function
  | `Yes ->
    In_thread.run (fun () -> Marshal.from_channel (Pervasives.open_in dump))
  | _ -> 
    p "Parsing textual source...";
    In_thread.run 
      (fun () ->
         let out = Pervasives.open_out dump in
         let chan = Pervasives.open_in src in
         let pol = NetKAT_Parser.program NetKAT_Lexer.token (Lexing.from_channel chan) in
         Marshal.to_channel out pol [];
         Pervasives.close_out out;
         pol)

(* Run by the worker to return the digest of the local copy of the policy *)
let dump_digest (expected_md5 : string) 
                (dump : string) () : bool Deferred.t = 
  try_with (fun () ->
    In_thread.run (fun () -> Digest.file dump))
  >>| function
  | Ok md5 -> 
     if md5 = expected_md5 then
        (p "policy available"; true)
      else
       false
  | Error _ -> false

let run_dump_digest md5 dump worker =
  Parallel.run ~buffer_age_limit  ~where:(`On worker) (dump_digest md5 dump)
  >>= function
  | Error exn ->
    return (p (sprintf "could not get digest from %s" worker); None)
  | Ok false ->
    return (Some worker)
  | Ok true ->
    return None

(* Runs on a worker to receive a policy and dump it to disk. *)
let receive_policy (pol_file : string) (pol_data : string) () : unit Deferred.t = 
  Writer.with_file pol_file (fun w ->
    Writer.write w pol_data;
    p "received policy";
    return ())

let compile_in_process ~pol_dump ~sw =
  p (sprintf "starting compile ~sw:%d ~pol:_" sw);
    let tbl_m = In_thread.run (fun () -> 
      let pol = Marshal.from_channel (Pervasives.open_in pol_dump) in
      LocalCompiler.to_table (LocalCompiler.compile (VInt.Int64 (Int64.of_int sw)) pol)) in
    Clock.every' ~stop:(tbl_m >>= fun _ -> return ()) (Time.Span.of_int_sec 5) (fun () -> p (heap_stats ()); return ());
    tbl_m >>= fun tbl ->
    let n = List.length tbl in
    p (sprintf "finished compile ~sw:%d ~pol:_ (flow table has length %d)" sw n);
    return n

let compile ~pol_dump ~sw = 
  measure_time (sprintf "compile ~sw:%d" sw) (fun () ->
    compile_in_process ~pol_dump ~sw)

let rec ship_policy (pol_file : string)
                    (pol_data : string) 
                    (worker : string) : unit Deferred.t =
  p (sprintf "Sending policy to %s" worker);
  try_with (fun () ->
    Parallel.run ~buffer_age_limit  ~where:(`On worker) (receive_policy pol_file pol_data)
    >>= function
    | Ok r -> p (sprintf "ship_policy to %s succeeded" worker); return ()
    | Error e -> p (sprintf "ship_policy to %s died with %s" worker e); return ())
    >>| function
    | Ok x -> x
    | Error exn -> 
      (p (sprintf "exception in ship_policy to %s; %s" worker (Exn.to_string exn));
       ())

let rec reader config =
  printf "> %!";
  Reader.read_line (force Reader.stdin)
  >>= function
  | `Eof -> Shutdown.shutdown 0; return ()
  | `Ok input ->
  let parsed_input = String.split input ~on:' ' in
  try_with (fun () ->
    match parsed_input with
    | [ "ship"; filename ] -> 
      ship config filename
    | [ "compile"; filename; m; n ] ->
      ((try return (Some (Int.of_string m, Int.of_string n)) with _ -> return None)
       >>= function
       | None -> return ()
       | Some (m, n) -> compile_all config filename m n)
    | _ -> return ())
  >>= function
  | Ok () -> reader config
  | Error exn ->
    printf "Exception:\n%s\n%!" (Exn.to_string exn);
    reader config

and ship config filename =
  measure_time "shipping data" (fun () ->
    let src = filename ^ ".kat" in
    let bin = filename ^ ".bin" in
    parse_caching src bin 
    >>= fun _ ->
    Reader.file_contents bin
    >>= fun bin_data ->
    Deferred.List.iter ~how:`Parallel ~f:(ship_policy bin bin_data) config.workers)

and compile_all config filename min_sw max_sw =
  let bin = filename ^ ".bin" in
  let switches = range min_sw max_sw in
  measure_time "compilation"
    (fun () -> 
       cluster_map switches ~workers:config.workers ~f:(fun sw -> compile ~pol_dump:bin ~sw))
  >>= fun results ->
  let failures = List.filter_map results ~f:Result.error in
  List.iter failures ~f:(fun exn -> p (Exn.to_string exn));
  printf "%d failures.\n%!" (List.length failures);
  return ()

let main () = 
  match Array.to_list Sys.argv with
    | [_; "local"; pol_dump; n] ->
      let sw = Int.of_string n in
      let _ = compile_in_process ~pol_dump ~sw in
      never_returns (Scheduler.go ())
    | [_; "master"; filename ] ->
      let config = parse_config filename in
      Parallel.init ~cluster: { Cluster.master_machine = Unix.gethostname();
                                worker_machines = config.workers } ();
      p "Master started";
      let _ = reader config in
      never_returns (Scheduler.go ())
    | [_] -> Parallel.init ()
    | _ -> printf "Invalid argument. See source code for help.\n%!"

let () = 
  try main ()
  with exn -> (
    let _ = Core.Std.Sys.command "touch /home/arjun/exn" in
  raise exn)
