open Async.Std

val start_static
  :  f:('a -> VInt.t -> SDN_Types.flowTable)
  -> port:int
  -> pol:'a
  -> unit Deferred.t
