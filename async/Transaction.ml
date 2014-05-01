open Core.Std
open Async.Std

module Controller = Async_OpenFlow.OpenFlow0x01.Controller
module ClientTbl = Hashtbl.Make(Controller.Client_id)

module Xid = Int32
module XidTbl = Hashtbl.Make(Int32)

type r = [ `Disconnect of Sexp.t | `Result of OpenFlow0x01.Message.t ]

type t = {
  ctl : Controller.t;
  mutable xid : Xid.t;
  pending : r Ivar.t XidTbl.t ClientTbl.t
}

let next_xid t =
  let xid = t.xid in
  t.xid <- Int32.(xid + 1l);
  xid

(** [send t c_id msg] will send [msg] to the specified client and return an
 * Ivar.t that will become determined when the response is received. *)
let send t c_id (msg : OpenFlow0x01.Message.t) =
  let xid = next_xid t in
  let ivar = Ivar.create () in
  let xids = ClientTbl.find_exn t.pending c_id in
  XidTbl.add_exn xids xid ivar;
  Controller.send t.ctl c_id (next_xid t, msg)
  >>| function
    | `Sent _   -> `Sent ivar
    | `Drop exn ->
      begin match ClientTbl.find t.pending c_id with
      | Some(xids) ->
        XidTbl.remove xids xid
      | None -> ()
      end;
      `Drop exn

let send_switch t sw_id (msg : OpenFlow0x01.Message.t) =
  send t (Controller.client_id_of_switch t.ctl sw_id) msg

(** [create ctl] returns a Transaction.t and a Stage.t. The Transaction.t is the
 * ctl object used to interact with the instance.
 *
 * The only public API is the send function, which will send an
 * OpenFlow0x01.Message.t to the specified client. The result is an Ivar.t that
 * will become determined when the response message is received.
 *
 * The return Stage.t should be sequenced with other stages that comprise the
 * controller being built. This stage listens for responses and fills the
 * appropriate Ivar.t
 * *)
let create ?(size=10) (ctl : Controller.t) =
  let t = { ctl; xid = 0l; pending = ClientTbl.create ~size () } in

  let handler t evt =
    begin match evt with
    | `Connect(c_id, feats) ->
      ClientTbl.add_exn t.pending c_id (XidTbl.create ~size ());
      return [evt]
    | `Disconnect(c_id, sw_id, exn_) ->
      begin match ClientTbl.find_and_remove t.pending c_id with
      | Some(xids) ->
        XidTbl.iter_vals xids ~f:(fun ivar -> Ivar.fill ivar (`Disconnect exn_))
      | None -> ()
      end;
      return [evt]
    | `Message(c_id, (xid, (msg : OpenFlow0x01.Message.t))) ->
      let xids = ClientTbl.find_exn t.pending c_id in
      begin match XidTbl.find_and_remove xids xid with
      | Some(ivar) ->
        Ivar.fill ivar (`Result msg);
        return []
      | None ->
        return [evt]
      end
    end
  in
  (t, handler)
