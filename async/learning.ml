open Core.Std
open Async.Std
open Packet
open SDN_Types
open Async_NetKAT
open NetKAT_Types

module Log = Async_OpenFlow.Log
module Hashtbl = Hashtbl.Poly
let tags = [("netkat", "learning")]

let table : (switchId * dlAddr, portId) Hashtbl.t = Hashtbl.create ()

let create () =

  let r_evts, w_evts = Pipe.create () in

  let learn switch_id port_id packet : unit =
    ignore (Hashtbl.add table ~key:(switch_id, packet.dlSrc) ~data:port_id);
    Deferred.don't_wait_for (Clock.after (Time.Span.of_sec 30.0)
      >>= fun () ->
      Hashtbl.remove table (switch_id, packet.dlSrc);
      Pipe.write w_evts Update) in

  let forward switch_id packet : action =
    match Hashtbl.find table (switch_id, packet.dlSrc) with
      | None -> OutputAllPorts
      | Some pt -> OutputPort pt in


  let default = Mod(Location(Pipe "learn")) in

  let gen_pol () =
    let drop = Filter False in
    let (fwd_pol, ctrl_pol) = Hashtbl.fold table ~init:(Filter False, True) 
      ~f:(fun ~key:(sw, eth) ~data:pt (k, u) ->
          let k' = Union(Seq(Filter(Test(EthDst eth)),
                             Mod(Location(Physical (VInt.get_int32 pt)))),
                         k) in
          let u' = Or(Test(EthDst eth), u) in
          (k', u')) in
      Union(Seq(Filter(Test(Switch sw)),
                Union(fwd_pol, Seq(Filter(Neg ctrl_pol), default))),
            acc) in

  let handler t w () = (r_evts, fun e -> match e with
    | PacketIn(_, switch_id, port_id, bytes, _, buf) ->
      let packet = Packet.parse bytes in
      learn switch_id port_id packet;
      return (Some(gen_pol ()))
    | Update ->
      return (Some(gen_pol ()))
    | _ -> return None) in
      
  create ~pipes:(PipeSet.singleton "learn") default handler
