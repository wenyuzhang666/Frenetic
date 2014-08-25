open Core.Std
open Async.Std

(** Implements a controller for ONF. *)

val start
  : Async_NetKAT.app
  -> ?port:int
  -> ?update:[`BestEffort | `PerPacketConsistent ]
  -> ?discovery:bool
  -> ?policy_queue_size:int
  -> unit -> unit
