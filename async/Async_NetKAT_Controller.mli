open Core.Std
open Async.Std


(** Start a NetKAT controller that is capable of managing OpenFlow 1.0 switches.

    The [policy_queue_size] specifies the maximum number of policy updates the
    controller is allowed to queue up before it is forced to install them on the
    network. As each policy update will eradicate the behavior of all previous
    update, the controller will simply install the latest policy and discard all
    preceding ones. In many situations this may improve the stability of network
    forwarding state.

    Note that while the controller will run with any combination of
    configuration options, some combinations will produces unexpected behavior.
    Specifically, consider the following combinaton

      [start app ~discovery:false ~update:`PerPacketConsistent ()]

    Consistent updates relies on topology discovery to determine the edge of the
    network. With discovery disbaled, every port on a switch is considered an
    edge port, with the result that version labels will never survive a version
    hop. This opens up the possiblity (in fact it almost ensures) that a packet
    will be subjected to more than policy version in its lifetime. For that
    reason, use this combination of configuration options at your own peril.
 *)
val start
  : Async_NetKAT.app
  -> ?port:int
  -> ?update:[`BestEffort | `PerPacketConsistent ]
  -> ?discovery:bool
  -> ?policy_queue_size:int
  -> unit -> unit
