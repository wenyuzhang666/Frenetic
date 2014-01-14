open Async.Std

val start
  :  f:('a -> VInt.t -> SDN_Types.flowTable)
  -> port:int
  -> init_pol:'a
  -> pols:'a Pipe.Reader.t
  -> unit Deferred.t

val start_static
  :  f:('a -> VInt.t -> SDN_Types.flowTable)
  -> port:int
  -> pol:'a
  -> unit Deferred.t
