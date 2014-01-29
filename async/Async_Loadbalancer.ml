open Core.Std
open Async.Std

module Controller = Async_OpenFlow.OpenFlow0x01.Controller
module M = Async_OpenFlow.OpenFlow0x01.Message
module T = Async_OpenFlow.Platform.Trans
module SDN = SDN_Types
module OF0x01 = OpenFlow0x01

module BalanceGroup = Map.Make(struct
  type t = (VInt.t * VInt.t) with sexp

  let compare (a0, a1) (b0, b1) =
    match VInt.compare a0 b0 with
      | 0 -> VInt.compare a1 b1
      | n -> n
end)

module SwitchMap = Map.Make(Controller.Client_id)


module Log = Async_OpenFlow.Log

let max_pending_connections = 64

let _ = Log.set_level `Debug

let _ = Log.set_output
          [Log.make_colored_filtered_output
             [("openflow", "socket");
              ("openflow", "platform");
              ("openflow", "serialization");
              ("openflow", "reactive")]]

let tags = [("openflow", "reactive")]

let partition flowTable =
  let priority = ref 65536 in
  let from_flow f =
    decr priority;
    SDN_OpenFlow0x01.from_flow !priority f in
  let static, balance = ref [], ref BalanceGroup.empty in
  List.iter flowTable ~f:(fun flow ->
    match flow.SDN.action with
      | [g] -> static := (from_flow flow) :: !static
      | gs  -> balance :=
        let open SDN in
        let open OpenFlow0x01_Core in
        try
          let key = (FieldMap.find EthSrc flow.pattern, FieldMap.find EthDst flow.pattern) in
          BalanceGroup.add !balance key
            (List.map gs ~f:(fun g -> from_flow { flow with action = [g] }))
        with Not_found ->
          failwith "Must specify src/dst mac address for loadbalance group");
  (!static, !balance)

module State = struct
  type sw_t = {
    static_flows : OpenFlow0x01_Core.flowMod list;
    balance_flows : OpenFlow0x01_Core.flowMod list BalanceGroup.t
  }

  type t = {
    local : VInt.t -> SDN_Types.flowTable;
    switches : (VInt.t * sw_t) SwitchMap.t
  }

  let create t = { local = t; switches = SwitchMap.empty }

  let add_switch t c_id feats =
    let open OF0x01 in
    let sw_id = VInt.Int64 feats.SwitchFeatures.switch_id in
    Log.info ~tags:tags "switch %s - connected%!"
      (VInt.get_string sw_id);
    let static_flows, balance_flows = partition (t.local sw_id) in
    ({ t with switches =
        SwitchMap.add t.switches ~key:c_id ~data:(sw_id, { static_flows; balance_flows }) },
     static_flows)

  let remove_switch t c_id =
    { t with switches = SwitchMap.remove t.switches c_id }

  let hash_packet pkt =
    let open Packet in
    let bits = Cstruct.create 14 in
    Cstruct.BE.set_uint16 bits 0 (Packet.dlTyp pkt);
    Cstruct.BE.set_uint32 bits 2 (Packet.nwSrc pkt);
    Cstruct.BE.set_uint32 bits 6 (Packet.nwDst pkt);
    Cstruct.BE.set_uint16 bits 10 (Packet.tpSrc pkt);
    Cstruct.BE.set_uint16 bits 12 (Packet.tpDst pkt);
    Crc.crc32 (Cstruct.to_string bits)

  let decide t c_id xid pi =
    let open OpenFlow0x01_Core in
    let pkt = parse_payload pi.input_payload in
    let sw_id, sw_t = SwitchMap.find_exn t.switches c_id in
    let src, dst = VInt.Int64 pkt.Packet.dlSrc, VInt.Int64 pkt.Packet.dlDst in
    match BalanceGroup.find sw_t.balance_flows (src, dst) with
      | Some (group) ->
        let flow = List.nth_exn group (let open Int64 in
          to_int_exn (rem (hash_packet pkt) (of_int_exn (List.length group)))) in
        let open OF0x01.Message in
        let open OpenFlow0x01_Core in
        begin match pi.input_payload with
          | Buffered(buf_id, _) ->
            [(0l, FlowModMsg { flow with apply_to_packet = Some(buf_id) })]
          | NotBuffered _ ->
            let open OF0x01.PacketOut in
            [(xid, PacketOutMsg {
                output_payload = pi.input_payload;
                port_id = Some(pi.port);
                apply_actions = [ Output(match flow.out_port with | Some(p) -> p |
                None -> failwith "should not happen") ] });
            (0l, FlowModMsg flow)]
          end
      | None -> []
end

let start_static ~f ~port ~pol : unit Deferred.t =
  Controller.create ~port ~max_pending_connections () >>= fun t ->
  Log.info "Controller started / Listening for switches";
  let evts = T.run Controller.features t (Controller.listen t) in
  let init = State.create (f pol) in

  Pipe.fold evts ~init ~f:(fun s evt ->
    match evt with
      | `Connect (c_id, feats) ->
        let open OpenFlow0x01_Core in
        let open OF0x01.Message in
        let open SDN_OpenFlow0x01 in
        let s', static = State.add_switch s c_id feats in
        let catch = add_flow 0 match_all [Output (Controller 1024)] in
        (* XXX(seliopou): This is ignoring send errors! *)
        Deferred.all
          (List.map (catch :: static) ~f:(fun m -> Controller.send t c_id (0l, FlowModMsg m)))
        >>| fun _ -> s'
      | `Disconnect(c_id,_) ->
        return (State.remove_switch s c_id)
      | `Message (c_id, (xid, msg)) ->
        let open OF0x01.Message in
        begin match msg with
          | PacketInMsg pktIn ->
            let balance = State.decide s c_id xid pktIn in
            Deferred.all
              (List.map balance ~f:(Controller.send t c_id))
            >>| fun _ -> s
          | _ ->
            Log.error "Dropped message: %s" (to_string msg);
            return s
        end)
  >>| fun _ -> ()
