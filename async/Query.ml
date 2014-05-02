open Core.Std
open Async.Std

open NetKAT_Types

module Net = Async_NetKAT.Net

let switchids nib =
  Net.Topology.fold_vertexes (fun v acc -> match Net.Topology.vertex_to_label nib v with
    | Async_NetKAT.Switch id -> id::acc
    | _ -> acc)
  nib []

let every
  (t   : Time.Span.t)
  (txn : Transaction.t)
  (nib : Net.Topology.t ref)
  (app : Async_NetKAT.app)
  : event Pipe.Reader.t =
  Pipe.init (fun w ->
    Deferred.never (Deferred.forever () (fun () ->
      Clock.after t >>= function () ->
      (* First iterate over all the outstanding queries, then iterate over all
       * the switches, collecting responses for each switch to satisfy the
       * query. Note that under this iteration pattern, each query may be
       * working on a different set of switches, which seems OK. *)
      Deferred.List.iter (Async_NetKAT.queries app)      ~f:(fun (query, pred) ->
      Deferred.List.fold (switchids !nib) ~init:(0L, 0L) ~f:(fun acc sw_id ->
        let patterns = NetKAT_LocalCompiler.pred_to_patterns sw_id pred in
        Deferred.List.fold patterns ~init:acc ~f:(fun (bytes, packets) pattern ->
          let open OpenFlow0x01_Stats in
          let open OpenFlow0x01.Message in
          let req = StatsRequestMsg(AggregateRequest {
            as_of_match = SDN_OpenFlow0x01.from_pattern pattern;
            as_table_id = 0;
            as_out_port = None
          }) in
          Transaction.send_switch txn sw_id req
          >>= function
            | `Drop exn -> raise exn
            | `Sent ivar -> Ivar.read ivar
              >>= function
                | `Disconnect exn ->
                  return (bytes, packets)
                | `Result (StatsReplyMsg (AggregateFlowRep resp)) ->
                  return Int64.(bytes + resp.total_byte_count, packets + resp.total_packet_count)
                | `Result _ ->
                  assert false)
      )
      >>= function (bytes, packets) ->
      Pipe.write w (Query(query, pred, bytes, packets)))
  )))
