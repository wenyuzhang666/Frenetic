open Core.Std
open Async.Std

module Controller = Async_OpenFlow.OpenFlow0x01.Controller
module M = Async_OpenFlow.OpenFlow0x01.Message
module T = Async_OpenFlow.Platform.Trans
module SDN = SDN_Types
module OF0x01 = OpenFlow0x01


module PortSet = Set.Make(VInt)
module SwitchMap = Map.Make(Controller.Client_id)

module Log = Async_OpenFlow.Log

let _ = Log.set_level `Debug

let _ = Log.set_output
          [Log.make_colored_filtered_output
             [("openflow", "socket");
              ("openflow", "platform");
              ("openflow", "serialization")]]

let filter_map ~f xs =
  let rec loop rs xs =
    match xs with
      | [] -> rs
      | x::xs' ->
        begin match f x with
          | None -> loop rs xs'
          | Some(r) -> loop (r::rs) xs'
        end in
  loop [] xs

let pick_group ports (par : SDN.par) =
  let f (a : SDN.action) =
    match a with
      | SDN.SetField (SDN.InPort, p) ->
        PortSet.mem ports p
      | _ -> false in
  if List.exists par ~f:(List.exists ~f:f) then
    Some(par)
  else
    None

let failover flow (ports : PortSet.t) : SDN.flow option =
  match filter_map (pick_group ports) flow.SDN.action with
    | [] -> None
    | g::gs -> Some({ flow with SDN.action = [g] })

let to_messages flowtable ports ~f =
  let open OF0x01.Message in
  let priority = ref 65536 in
  let delete = f (0l, FlowModMsg OpenFlow0x01_Core.delete_all_flows) in
  let adds = List.map flowtable ~f:(fun flow ->
    let specialized = match failover flow ports with
      | None -> failwith "can't specialize flow"
      | Some(f) -> f in
    decr priority;
    f (0l, FlowModMsg (SDN_OpenFlow0x01.from_flow !priority specialized))) in
  delete :: adds

let update_flow_table t sw_id = failwith "NYI"

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

  type ('e, 'p) t = {
    local : VInt.t -> SDN_Types.flowTable;
    sws : (VInt.t * PortSet.t) SwitchMap.t;
    e : 'e Deferred.choice option;
    p : 'p Deferred.choice option
  }

  let create local = {
    local;
    sws = SwitchMap.empty;
    e = None;
    p = None
  }

  let add_switch s ~c_id ~feats =
    let open OF0x01 in
    let sw_id = VInt.Int64 feats.SwitchFeatures.switch_id in
    let ports = PortSet.of_list
      (List.filter_map feats.SwitchFeatures.ports ~f:(fun p ->
        if PortDescription.(p.config.PortConfig.down)
          then None
          else Some(VInt.Int16(p.PortDescription.port_no)))) in
    ({ s with sws = SwitchMap.add s.sws ~key:c_id ~data:(sw_id, ports) },
     (sw_id, ports))

  let remove_switch s ~c_id =
    { s with sws = SwitchMap.remove s.sws c_id }

  let add_port s ~c_id ~desc =
    let sw_id, ports = SwitchMap.find_exn s.sws c_id in
    let ports' = PortSet.add ports
      (VInt.Int16(OF0x01.PortDescription.(desc.port_no))) in
    ({ s with sws = SwitchMap.add s.sws ~key:c_id ~data:(sw_id, ports') },
     (sw_id, ports'))

  let remove_port s ~c_id ~desc =
    let (sw_id, ports) = SwitchMap.find_exn s.sws c_id in
    let ports' = PortSet.remove ports
      (VInt.Int16(OF0x01.PortDescription.(desc.port_no))) in
    ({ s with sws = SwitchMap.add s.sws ~key:c_id ~data:(sw_id, ports') },
     (sw_id, ports'))

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
    Deferred.choose (filter_map (fun e -> e) [ s.State.e ; s.State.p ])
    >>= function
      | `Event Some(evt) ->
        begin match evt with
          | `Connect(c_id, feats) ->
            let s', (sw_id, ports) = State.add_switch s c_id feats in
            let local_pol = s.State.local sw_id in
            Deferred.all (to_messages local_pol ports ~f:(Controller.send t c_id))
            >>| fun _ -> { s' with
              State.e = Some(choose_event t evts)
            }
          | `Disconnect(c_id, _) ->
            return (State.remove_switch s c_id)
          | `Message(c_id, msg) ->
            let open OF0x01.Message in
            let open OF0x01.PortStatus in
            begin match msg with
              | _, PortStatusMsg { reason = ChangeReason.Add; desc } ->
                let s', (sw_id, ports) = State.add_port s c_id desc in
                let local_pol = s'.State.local sw_id in
                Deferred.all (to_messages local_pol ports ~f:(Controller.send t c_id))
                >>| (fun _ -> s')
              | _, PortStatusMsg { reason = ChangeReason.Delete; desc } ->
                let s', (sw_id, ports) = State.remove_port s c_id desc in
                let local_pol = s'.State.local sw_id in
                Deferred.all (to_messages local_pol ports ~f:(Controller.send t c_id))
                >>| (fun _ -> s')
              | _, PortStatusMsg { reason = ChangeReason.Modify }
              | _ ->
                Log.info "Dropped message: %s" (M.to_string msg);
                return s
            end
        end
      | `Policy Some(new_pol) ->
        let local = f new_pol in
        let next ~key ~data =
          let sw_id, ports = data in
          Deferred.all (to_messages (local sw_id) ports ~f:(Controller.send t key))
          >>| (fun _ -> ()) in
        Deferred.Map.iter s.State.sws ~f:next
        >>= fun _ -> return { s with
          State.local = local;
          State.p = Some(choose_policy t pols)
        }
      | `Event None ->
        return { s with State.e = None }
      | `Policy None ->
        return { s with State.p = None });
    Deferred.unit

let start_static ~f ~port ~pol : unit Deferred.t =
  start f port pol (Async.Std.Pipe.of_list [])
