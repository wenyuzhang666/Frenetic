open Core.Std
open Async.Std

module Net = Async_NetKAT.Net

module Log = Async_OpenFlow.Log
let tags = [("netkat", "traffic")]

let create () =
  let open Async_NetKAT in
  let open NetKAT_Types in

  let queries = ref [] in

  let to_query dl sw_id =
    ("traffic", And(Test(EthDst(dl)), Test(Switch(sw_id)))) in

  let gen_queries nib () =
    queries := Net.Topology.fold_vertexes (fun v acc ->
      let open Net.Topology in
      match vertex_to_label nib v with
      | Async_NetKAT.Host(dl, nw) ->
        begin match next_hop nib v 0l with
        | Some(e) ->
          let v', pt = edge_dst e in
          begin match vertex_to_label nib v' with
          | Async_NetKAT.Switch(sw_id) ->
            (to_query dl sw_id)::acc
          | _ -> acc
          end
        | None -> acc
        end
      | _ -> acc)
    nib [];
    !queries in

  let handler t w () e =
    match e with
    | Query("traffic", pred, bytes, packet) ->
      begin match pred with
      | And(Test(EthDst(dl)), Test(Switch(sw_id))) ->
        Log.info ~tags "[traffic] { dlDst = %s, switch = %Lu, bytes = %Lu, packet = %Lu }"
          (Packet.string_of_mac dl) sw_id bytes packet;
        Log.flushed () >>= fun () ->
        return (None, !queries)
      | _ ->
        return (None, !queries)
      end
    | HostUp _
    | HostDown _ ->
      return (None, gen_queries !t ())
    | _ -> return (None, !queries) in

  create (Filter False) [] handler
