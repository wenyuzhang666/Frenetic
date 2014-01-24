open Core.Std
open Async.Std
open Async_parallel.Std

module Types = NetKAT_Types
(* 

 An example configuration:

{
  workers: [ "10.152.136.136" ]
  per_worker: 1
}

*)

(*

  Compiling switch 0 [size=7347204]...Done [ctime=3.747614s ttime=0.000001s tsize=0]@
Compiling switch 1 [size=7347204]...Done [ctime=3.715512s ttime=0.001282s tsize=80]@
Compiling switch 2 [size=7347204]...Done [ctime=3.727948s ttime=0.001186s tsize=80]@
arjun@frenetic:~/src/frenetic$ ./katnetic.native dump local stats 2 abfattree-tables-fail-local-16-3.kat 2> /dev/null
Compiling switch 0 [size=7347204]...Done [ctime=3.685086s ttime=0.000001s tsize=0]@
Compiling switch 1 [size=7347204]...Done [ctime=3.698021s ttime=0.001268s tsize=80]@
Compiling switch 2 [size=7347204]...Done [ctime=3.702919s ttime=0.001150s tsize=80]@

*)
type config = {
  workers: string list;   (* hostnames or IP addresses of worker machines  *)
  per_worker : int;
}

let parse_pol filename = 
  let chan = Pervasives.open_in filename in
  let pol = NetKAT_Parser.program NetKAT_Lexer.token (Lexing.from_channel chan) in
  Pervasives.close_in chan;
  pol

let parse_config (filename : string) : config = 
  let json = Yojson.Basic.from_file filename in
  let open Yojson.Basic.Util in
  let workers = json |> member "workers" |> to_list |> filter_string in
  let per_worker = json |> member "per_worker" |> to_int in
  { workers; per_worker }

let rec range (min : int) (max : int) : int list =
  if min = max then [max] else min :: range (min + 1) max

let p s = Printf.eprintf "%s:%s: %s\n%!" (Unix.gethostname ())
  (Pid.to_string (Unix.getpid ())) s

exception Remote_exception of string * string

let measure_time label (f : unit -> 'a Deferred.t) : 'a Deferred.t =
  let start = Time.now () in
  f ()
  >>= fun r ->
  let end_ = Time.now () in
  let span = Time.diff end_ start  in
  let n = Core.Std.Gc.allocated_bytes () /. 1024. /. 1024. in
  p (sprintf "%s took %s (%f MB)" label (Time.Span.to_string span) n);
  return r

let cluster_map (lst : 'a list) 
  ~(per_worker : int)
  ~(config : config) 
  ~(f : string -> 'a -> 'b Deferred.t) : ('b, exn) Result.t list Deferred.t =
  let (workers_r, workers_w) = Pipe.create () in
  let workers = List.concat_map (range 1 per_worker) 
                  ~f:(fun _ -> config.workers) in
  p (sprintf "running %d jobs in parallel" (List.length workers));
  List.iter workers ~f:(Pipe.write_without_pushback workers_w);
  let run_on_worker x =
    Pipe.read workers_r
    >>= function
    | `Eof -> failwith "unexpected EOF from cluster_map worker pipe"
    | `Ok worker -> 
       try_with
         (fun () -> Parallel.run ~where:(`On worker) (fun () -> f worker x))
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

(* Runs on a worker to receive a policy and dump it to disk. *)
let receive_policy (pol_data : string) () : string Deferred.t = 
  let (tmp_path, out_chan) = Filename.open_temp_file "frenetic" "kat" in
  let writer = Writer.of_out_channel out_chan Fd.Kind.File in
  Writer.write writer pol_data;
  Writer.close writer >>=
  fun () ->
  (* Closing out_chan appears to be unnecessary *)
  return tmp_path

let compile_in_process ~pol_dump ~sw =
  p (sprintf "starting compile ~sw:%d ~pol:_" sw);
  let stats_m = In_thread.run (fun () -> 
    let open LocalCompiler in
    let sw_vint = VInt.Int64 (Int64.of_int sw) in
    let pol = Marshal.from_channel (Pervasives.open_in pol_dump) in
    let t1 = Unix.gettimeofday () in
    let i = compile sw_vint pol in
    let t2 = Unix.gettimeofday () in
    let tbl = to_table i in
    let t3 = Unix.gettimeofday () in
    (t1, t2, t3, tbl, Semantics.size pol)) in
  let completed = stats_m >>= fun _ -> return () in
  Clock.every' (Time.Span.of_int_sec 30) ~stop:completed
    (fun () ->
       let n = Core.Std.Gc.allocated_bytes () /. 1024. /. 1024. in
       p (sprintf "still running (%fMB heap)" n);
       return ());
  stats_m
  >>= fun (t1, t2, t3, tbl, pol_size) ->
  let n = List.length tbl in
  p (sprintf "finished compile ~sw:%d (flow table has length %d)" sw n);
  (* Output that Marco's scripts expect *)
  let str = sprintf
    "Compiling switch %d [size=%d]...Done [ctime=%fs ttime=%fs tsize=%d]"
    sw pol_size (t2 -. t1) (t3 -. t2) n in
  return str

let compile ~pol_dump ~sw = 
  measure_time (sprintf "compile ~sw:%d" sw) (fun () ->
    compile_in_process ~pol_dump ~sw)

let rec ship_policy tbl
                    (pol_data : string) 
                    (worker : string) : unit Deferred.t =
  p (sprintf "Sending policy to %s" worker);
  try_with (fun () ->
    Parallel.run ~where:(`On worker) (receive_policy pol_data)
    >>= function
    | Ok remote_file -> 
      p (sprintf "ship_policy to %s succeeded" worker);
      (* Returned result indicates if it is a duplicate entry. *)
      let _ = Hashtbl.add tbl ~key:worker ~data:remote_file in
      return ()
    | Error e -> p (sprintf "ship_policy to %s died with %s" worker e); return ())
    >>| function
    | Ok x -> x
    | Error exn -> 
      (p (sprintf "exception in ship_policy to %s; %s" worker (Exn.to_string exn));
       ())

and ship config tbl pol =
  measure_time "shipping data" (fun () ->
    In_thread.run  (fun () -> Marshal.to_string pol [])
    >>= fun bin_data ->
    Deferred.List.iter config.workers 
    ~how:`Parallel ~f:(ship_policy tbl bin_data))

and compile_all config cached_policies (per_worker : int) min_sw max_sw =
  let switches = range min_sw max_sw in
  measure_time "compilation"
    (fun () -> 
       cluster_map switches ~config ~per_worker 
         ~f:(fun worker sw ->
               let pol_dump = Hashtbl.find_exn cached_policies worker in
               compile ~pol_dump ~sw))
  >>= fun results ->
  let failures = List.filter_map results ~f:Result.error in
  List.iter failures ~f:(fun exn -> p (Exn.to_string exn));
  p (sprintf "%d failures.\n%!" (List.length failures));
  printf "%s\n%!" (String.concat (List.filter_map results ~f:Result.ok) ~sep:"\n");
  return ()

let rm_tmp cached_policies worker = 
  Parallel.run ~where:(`On worker)
    (fun () -> Unix.remove (Hashtbl.find_exn cached_policies worker))
  >>= function
    | Ok () -> return ()
    | Error exn ->
      (p (sprintf "rm_tmp exception: %s" exn));
      return ()
(* 
let dist_compiler (pol : Types.policy)
  ~(min_sw : int)
  ~(max_sw : int)
  ~(config : config) : unit Deferred.t =
  let per_worker = config.per_worker in
  (* Map from remote hostname to remote filename. *)
  let cached_policies = String.Table.create () in
  let ship_and_compile () = 
    ship config cached_policies pol_file 
    >>= fun () ->
    compile_all config cached_policies per_worker min_sw max_sw in
  let finally_block () = 
    Deferred.List.iter config.workers
      ~how:`Parallel ~f:(rm_tmp cached_policies) in
  Monitor.protect ship_and_compile finally_block
 *)
let main () = 
  match Array.to_list Sys.argv with
    | [_; "master"; config_file; pol_file; min_sw; max_sw ] ->
      let config = parse_config config_file in
      let per_worker = config.per_worker in
      let min_sw = Int.of_string min_sw in
      let max_sw = Int.of_string max_sw in
      Parallel.init ~cluster: { Cluster.master_machine = Unix.gethostname();
                                worker_machines = config.workers } ();
      (* Map from remote hostname to remote filename. *)
      let cached_policies = String.Table.create () in
      let ship_and_compile () = 
        ship config cached_policies (parse_pol pol_file) 
        >>= fun () ->
        compile_all config cached_policies per_worker min_sw max_sw in
      let finally_block () = 
        Deferred.List.iter config.workers
          ~how:`Parallel ~f:(rm_tmp cached_policies) in
      let _ =
        Monitor.protect ship_and_compile finally_block
        >>= fun () ->
        Shutdown.exit 0 in
      p "Master started";
      never_returns (Scheduler.go ())
    | _ ->
      if Parallel.is_worker_machine () then Parallel.init ()
      else printf "Invalid argument. See source code for help.\n%!"

let () = main ()
