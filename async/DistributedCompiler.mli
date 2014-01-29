(** Distributed compiler for Frenetic *)

open Core.Std
open Async.Std

(**

 An example configuration:

{
  workers: [ "10.152.136.136" ]
  per_worker: 10;
}

*)
type config = {
  workers: string list;   (* hostnames or IP addresses of worker machines  *)
  per_worker: int;        (* number of jobs to run per worker *)
}

(** Parses a configuration from a file *)
val parse_config : string -> config


(** Runs the distributed compiler. Debugging output prints to stderr and
    benchmarking output prints to stdout. The computation returns when the
    compilation completes. Right now, the actual policy is not returned.

    Assumes that [Parallel.init] has already been invoked and that all workers
    are part of the cluster. *)
val dist_compiler : NetKAT_Types.policy -> 
  min_sw:int ->
  max_sw:int ->
  config:config ->
  unit Deferred.t
