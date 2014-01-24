open Core.Std
open Async.Std

module Controller = Async_OpenFlow.OpenFlow0x01.Controller
module M = Async_OpenFlow.OpenFlow0x01.Message
module T = Async_OpenFlow.Platform.Trans
module SDN = SDN_Types
module OF0x01 = OpenFlow0x01


module PortSet = Set.Make(VInt)
module PortMap = Map.Make(VInt)
module SwitchMap = Map.Make(Controller.Client_id)

module Log = Async_OpenFlow.Log

let _ = Log.set_level `Debug

let _ = Log.set_output
          [Log.make_colored_filtered_output
             [("openflow", "socket");
              ("openflow", "platform");
              ("openflow", "serialization");
              ("openflow", "reactive")]]

let tags = [("openflow", "reactive")]

let choose_event t evts =
  let op = Pipe.read evts
    >>| function
      | `Eof -> None
      | `Ok evt -> Some(evt) in
  choice op (fun e -> `Event(e))

let choose_policy t pols =
  let op = Pipe.read pols
    >>| function
      | `Eof -> None
      | `Ok local -> Some(local) in
  choice op (fun e -> `Policy(e))

module State = struct

  type sw_t = {
    sw_id : VInt.t;
    live_ports : PortSet.t;
    port_flows : OF0x01.FlowMod.t list PortMap.t
  }

  type ('e, 'p) t = {
    local : VInt.t -> SDN.flowTable;
    sws : sw_t SwitchMap.t;
    e : 'e Deferred.choice option;
    p : 'p Deferred.choice option
  }

  let create local = {
    local;
    sws = SwitchMap.empty;
    e = None;
    p = None
  }

  let de_group flowtable =
    let priority = ref 65536 in
    let non_failover = ref [] in
    let port_flows = List.fold flowtable ~init:PortMap.empty ~f:(fun acc flow ->
      List.fold flow.SDN.action ~init:acc ~f:(fun acc par ->
        (* XXX(seliopou): This code assumes that the parallel components of each
         * group will send packets out the _same_ port. Without this assumption,
         * you must compute flows for each possible subset of the ports on the
         * switch.
         * *)
        let ports = PortSet.union_list (List.map par ~f:(fun seq ->
          List.fold seq ~init:PortSet.empty ~f:(fun port_acc action ->
            match action with
              | SDN.SetField (SDN.InPort, p)
              | SDN.OutputPort p
              | SDN.Enqueue (p, _) ->
                PortSet.add port_acc p
              | _ -> port_acc))) in
        assert (PortSet.length ports <= 1);
        decr priority;
        let flow = SDN_OpenFlow0x01.from_flow !priority
          { flow with SDN.action = [par] } in
        if PortSet.length ports = 0 then begin
          (* The action does not refer to any ports, so this rule should always
           * be installed *)
          non_failover := flow :: !non_failover;
          acc
        end else begin
          assert (PortSet.length ports = 1);
          let port = PortSet.min_elt_exn ports in
          let table = match PortMap.find acc port with
            | None -> []
            | Some(e) -> e in
          PortMap.add acc ~key:port ~data:(flow::table)
        end)) in
    (port_flows, !non_failover)

  let add_switch s ~c_id ~feats =
    let open OF0x01 in
    let sw_id = VInt.Int64 feats.SwitchFeatures.switch_id in
    let live_ports = PortSet.of_list
      (List.filter_map feats.SwitchFeatures.ports ~f:(fun p ->
        if PortDescription.(p.config.PortConfig.down)
          then None
          else Some(VInt.Int16(p.PortDescription.port_no)))) in
    Log.info ~tags:tags "switch %s - connected%!"
      (VInt.get_string sw_id);
    Log.info ~tags:tags "switch %s - ports: %s%!"
      (VInt.get_string sw_id)
      (PortSet.fold live_ports ~init:"" ~f:(fun acc e ->
        Printf.sprintf "%s%s"
        (VInt.get_string e)
        (if acc = "" then "" else ", " ^ acc)));
    let port_flows, non_failover = de_group (s.local sw_id) in
    let flows = List.concat (PortMap.data (PortMap.filter port_flows ~f:(fun ~key ~data ->
      PortSet.mem live_ports key))) in
    let switch = { sw_id; live_ports; port_flows } in
    ({ s with sws = SwitchMap.add s.sws ~key:c_id ~data:switch },
     (sw_id, non_failover @ flows))

  let remove_switch s ~c_id =
    { s with sws = SwitchMap.remove s.sws c_id }

  let add_port s ~c_id ~desc =
    let { sw_id; live_ports; port_flows } = SwitchMap.find_exn s.sws c_id in
    let port = VInt.Int16(OF0x01.PortDescription.(desc.port_no)) in
    let live_ports = PortSet.add live_ports port in
    Log.info "switch %s - add port %s"
      (VInt.get_string sw_id)
      (VInt.get_string port);
    let flows = match PortMap.find port_flows port with
      | None -> []
      | Some(flows) -> flows in
    let switch = { sw_id; live_ports; port_flows } in
    ({ s with sws = SwitchMap.add s.sws ~key:c_id ~data:switch },
     flows)

  let remove_port s ~c_id ~desc =
    let { sw_id; live_ports; port_flows } = SwitchMap.find_exn s.sws c_id in
    let port = VInt.Int16(OF0x01.PortDescription.(desc.port_no)) in
    let live_ports = PortSet.remove live_ports port in
    Log.info "switch %s - remove port %s"
      (VInt.get_string sw_id)
      (VInt.get_string port);
    let open OpenFlow0x01_Core in
    let flows = match PortMap.find port_flows port with
      | None -> []
      | Some(flows) ->
        List.map flows ~f:(fun flow -> { flow with command = DeleteStrictFlow }) in
    let switch = { sw_id; live_ports; port_flows } in
    ({ s with sws = SwitchMap.add s.sws ~key:c_id ~data:switch },
     flows)
end

let start ~f ~port ~init_pol ~pols =
  Controller.create ~port () >>= function t ->
  Log.info "Listening for switches";
  let evts = T.run Controller.features t (Controller.listen t) in
  let init_state = { (State.create (f init_pol)) with
    State.e = Some(choose_event t evts);
    State.p = Some(choose_policy t pols)
  } in

  Deferred.forever init_state (fun s ->
    Deferred.choose (List.filter_map ~f:(fun e -> e) [ s.State.e ; s.State.p ])
    >>= function
      | `Event Some(evt) ->
        let open OF0x01.Message in
        begin match evt with
          | `Connect(c_id, feats) ->
            let s', (sw_id, flows) = State.add_switch s c_id feats in
            Log.info ~tags:tags "switch %s - initializing%!"
              (VInt.get_string sw_id);
            Deferred.all (List.map flows ~f:(fun flow ->
              Controller.send t c_id (0l, FlowModMsg flow)))
            >>| fun _ -> s'
          | `Disconnect(c_id, _) ->
            return (State.remove_switch s c_id)
          | `Message(c_id, msg) ->
            let open OF0x01 in
            let open Message in
            let open PortStatus in
            begin match msg with
              | _, PortStatusMsg { reason = ChangeReason.Add; desc } ->
                let s', flows = State.add_port s c_id desc in
                Deferred.all (List.map flows ~f:(fun flow ->
                  Controller.send t c_id (0l, FlowModMsg flow)))
                >>| (fun _ -> s')
              | _, PortStatusMsg { reason = ChangeReason.Modify; desc }
                  when not PortDescription.(desc.state.PortState.down) ->
                let s', flows = State.add_port s c_id desc in
                Deferred.all (List.map flows ~f:(fun flow ->
                  Controller.send t c_id (0l, FlowModMsg flow)))
                >>| (fun _ -> s')
              | _, PortStatusMsg { reason = ChangeReason.Modify; desc }
                  when PortDescription.(desc.state.PortState.down) ->
                let s', flows = State.remove_port s c_id desc in
                Deferred.all (List.map flows ~f:(fun flow ->
                  Controller.send t c_id (0l, FlowModMsg flow)))
                >>| (fun _ -> s')
              | _, PortStatusMsg { reason = ChangeReason.Delete; desc } ->
                let s', flows = State.remove_port s c_id desc in
                Deferred.all (List.map flows ~f:(fun flow ->
                  Controller.send t c_id (0l, FlowModMsg flow)))
                >>| (fun _ -> s')
              | _ ->
                Log.info "Dropped message: %s" (M.to_string msg);
                return s
            end
        end >>| fun s -> { s with State.e = Some(choose_event t evts) }
      | `Policy Some(new_pol) ->
        (* XXX(seliopou): This is a rarely-used case and in fact will never be
         * hit during tests or experiments, so leave it unimplemented unitl it's
         * required. Just remember to block the event loop until all the new
         * flowtables have been installed on switches, or otherwise avoid the
         * controller trying to install two different policies on the network.
         *
         * Handshakes and echoes will still go through in the layer below.
         * *)
        failwith "NYI"
      | `Event None ->
        return { s with State.e = None }
      | `Policy None ->
        return { s with State.p = None });
    Deferred.unit

let start_static ~f ~port ~pol : unit Deferred.t =
  start f port pol (Async.Std.Pipe.of_list [])
