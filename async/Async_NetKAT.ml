open Core.Std
open Async.Std

open NetKAT_Types

exception Assertion_failed of string

type node =
    | Switch of SDN_Types.switchId
    | Host of Packet.dlAddr * Packet.nwAddr

module Node = struct
  type t = node

  let compare = Pervasives.compare

  let to_string t = match t with
    | Switch(sw_id)       -> Printf.sprintf "switch %Lu" sw_id
    | Host(dlAddr, nwAddr) -> Printf.sprintf "host %s/%s"
        (Packet.string_of_nwAddr nwAddr)
        (Packet.string_of_dlAddr dlAddr)

  let parse_dot _ _ = failwith "NYI: Node.parse_dot"
  let parse_gml _ = failwith "NYI: Node.parse_dot"

  let to_dot _ = failwith "NYI: Node.to_dot"
end

module Link = struct
  type t = unit

  let compare = Pervasives.compare

  let to_string () = "()"
  let default = ()

  let parse_dot _ = failwith "NYI: Link.parse_dot"
  let parse_gml _ = failwith "NYI: Link.parse_dot"

  let to_dot _ = failwith "NYI: Link.to_dot"
end

module Net = Network.Make(Node)(Link)

module PipeSet = Set.Make(struct
  type t = string with sexp
  let compare = Pervasives.compare
end)

exception Sequence_error of PipeSet.t * PipeSet.t

type query = string * NetKAT_Types.pred
type result = policy option * query list
type 'a handler = Net.Topology.t ref -> (switchId * SDN_Types.pktOut) Pipe.Writer.t -> unit -> event -> 'a Deferred.t

type app = {
  pipes : PipeSet.t;
  handler : result handler;
  mutable policy : policy;
  mutable queries : query list
}

let create ?pipes policy queries handler : app =
  let pipes = match pipes with
    | None -> PipeSet.empty
    | Some(pipes) -> pipes in
  { pipes; policy; handler; queries }

let create_without_queries ?pipes policy handler : app =
  create ?pipes policy [] (fun t w () ->
    let h' = handler t w () in
    fun e ->
      h' e >>| (fun r -> (r, [])))

let create_static (pol : policy) : app =
  create_without_queries pol (fun _ _ () _ -> return None)

let create_from_string (str : string) : app =
  let pol = NetKAT_Parser.program NetKAT_Lexer.token (Lexing.from_string str) in
  create_static pol

let create_from_file (filename : string) : app =
  let pol = In_channel.with_file filename ~f:(fun chan ->
    NetKAT_Parser.program NetKAT_Lexer.token (Lexing.from_channel chan)) in
  create_static pol

let policy (a : app) : policy =
  a.policy

let run
    (a : app)
    (t : Net.Topology.t ref)
    (w : (switchId * SDN_Types.pktOut) Pipe.Writer.t)
    (_ : unit)
    : event -> result Deferred.t =
  let a' = a.handler t w () in
  fun e -> match e with
    | PacketIn(p, _, _, _, _) when not (PipeSet.mem a.pipes p) ->
      return (None, a.queries)
    | _ ->
      a' e >>| fun (m_pol, queries) ->
        begin match m_pol with
          | Some(pol) -> a.policy <- pol
          | None -> ()
        end;
        a.queries <- queries;
        (m_pol, queries)

let union ?(how=`Parallel) (a1 : app) (a2 : app) : app =
  { pipes = PipeSet.union a1.pipes a2.pipes
  ; policy = Union(a1.policy, a2.policy)
  (* XXX(seliopou): for large compositions of applications, this should get
   * expensive. Use a dlist here to avoid the potential quadratic running
   * time. *)
  ; queries = List.append a1.queries a2.queries
  ; handler = fun t w () ->
      let a1' = run a1 t w () in
      let a2' = run a2 t w () in
      fun e ->
        Deferred.List.map ~how:how ~f:(fun a -> a e) [a1'; a2']
        >>= function
          | [(m_pol1, queries1); (m_pol2, queries2)] ->
            let m_pol = match m_pol1, m_pol2 with
              | None, None ->
                None
              | Some(pol1), Some(pol2) ->
                (Some(Union(pol1, pol2)))
              | Some(pol1), None ->
                (Some(Union(pol1, a2.policy)))
              | None, Some(pol2) ->
                (Some(Union(a1.policy, pol2)))
            in
            return (m_pol, List.append queries1 queries2)
          | _ -> raise (Assertion_failed "Async_NetKAT.union: impossible length list")
  }

let seq (a1 : app) (a2: app) : app =
  begin if not PipeSet.(is_empty (inter a1.pipes a2.pipes)) then
    (* In order for the form of composition below, the apps must not be
     * listening on the same pipe for `PacketIn` events. In this case,
     * only one of the apps will actually run and produce PacketOut messages
     * on a `PacketIn` event. *)
    raise (Sequence_error(a1.pipes, a2.pipes))
  end;
  { pipes = PipeSet.union a1.pipes a2.pipes
  ; policy = Seq(a1.policy, a2.policy)
  ; queries = List.append a1.queries a2.queries
  ; handler = fun t w () ->
      let a1' = run a1 t w () in
      let a2' = run a2 t w () in
      fun e ->
        a1' e >>= fun (m_pol1, queries1) ->
        a2' e >>= fun (m_pol2, queries2) ->
          let m_pol = match m_pol1, m_pol2 with
            | None, None ->
              None
            | Some(pol1), Some(pol2) ->
              (Some(Seq(pol1, pol2)))
            | Some(pol1), None ->
              (Some(Seq(pol1, a2.policy)))
            | None, Some(pol2) ->
              (Some(Seq(a1.policy, pol2)))
          in
          return (m_pol, List.append queries1 queries2)
  }
