(** The NetCore policy language *)
open OpenFlow0x01.Types
open Packet.Types

module Syntax : sig

  type get_packet_handler = switchId -> portId -> packet -> unit

  type predicate =
    | And of predicate * predicate
    | Or of predicate * predicate
    | Not of predicate
    | All
    | NoPackets
    | Switch of switchId
    | InPort of portId
    | DlSrc of Int64.t
    | DlDst of Int64.t
    | DlVlan of int option
    | SrcIP of Int32.t
    | DstIP of Int32.t
    | TcpSrcPort of int (** 16-bits, implicitly IP *)
    | TcpDstPort of int (** 16-bits, implicitly IP *)

  type action =
    | To of int
    | ToAll
    | UpdateDlSrc of Int64.t * Int64.t
    | UpdateDlDst of Int64.t * Int64.t
    | UpdateDlVlan of int option * int option
    | GetPacket of get_packet_handler

  type policy =
    | Empty
    | Act of action
    | Par of policy * policy (** parallel composition *)
    | Seq of policy * policy
    | Filter of predicate
    | Slice of predicate * policy * predicate

  val par : policy list -> policy

  val predicate_to_string : predicate -> string
  val action_to_string : action -> string
  val policy_to_string : policy -> string

  (** Desugars the surface syntax policy to the internal (Coq-extracted) policy
      and a hashtable of handlers. *)
  val desugar : 
    (unit -> int) -> 
    (unit -> int option) -> 
    policy 
    -> (int, get_packet_handler) Hashtbl.t 
    -> NetCoreEval.pol

end

(** [start_controller port policy] *)
val start_controller : int -> Syntax.policy Lwt_stream.t -> unit

(** The NetCore controller. *)
 module Make : functor (Platform : OpenFlow0x01.PLATFORM) -> sig
  val start_controller : Syntax.policy Lwt_stream.t -> unit Lwt.t
 end