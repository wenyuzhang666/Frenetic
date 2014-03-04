(* TODO: Use MongoDB. *)
open Core.Std
open Async.Std
open NetKAT_Types
open Async_NetKAT

module Log = Async_OpenFlow.Log

cstruct header {
  uint16_t code;
  uint32_t size;
} as little_endian

let send_json_over_tcp (w : Writer.t) (json : Yojson.json) : unit =
  let str = Yojson.to_string json in
  let header = Cstruct.create sizeof_header in
  set_header_code header 206;
  set_header_size header (Int32.of_int_exn (String.length str));
  Writer.write w (Cstruct.to_string header);
  Writer.write w str

let recv_json_over_tcp (r : Reader.t) : Yojson.Basic.json Deferred.t =
  let buf = String.create sizeof_header in
  Reader.really_read r buf
  >>= function
  | `Eof _ -> failwith "application disconnected (reading header)"
  | `Ok -> 
  let buf = String.create (Int32.to_int_exn (get_header_size (Cstruct.of_string buf))) in
  Reader.really_read r buf
  >>= function
  | `Eof _ -> failwith "application disconnected (reading body)"
  | `Ok -> return (Yojson.Basic.from_string buf)

let make ~(host:string) ~(port:int)
         ~(pipes:PipeSet.t) : app Deferred.t = 
  Tcp.connect (Tcp.to_host_and_port host port)
  >>= fun (sock, sock_reader, sock_writer) ->
  Log.info "Connected to remote application";
  let (client_event_reader, client_event_writer) = Pipe.create () in
  let client_fun (event : event) : result Deferred.t = 
    send_json_over_tcp sock_writer (event_to_json event);
    recv_json_over_tcp sock_reader
    >>= function
    | `Null -> return None
    | json -> 
      let pol = json_to_pol json in
      Log.info "policy from remote: %s" (NetKAT_Pretty.string_of_policy pol);
      return (Some pol) in
  (* Ignoring topology and packet-out handler for now *)
  let handler _ _ () = (client_event_reader, client_fun) in
  client_fun Update
  >>= function
  | None -> return (create ~pipes NetKAT_Types.drop handler)
  | Some init_pol -> return (create ~pipes init_pol handler)
