(** Utilities to manipulate the airspace configuration *)

(** Specification of a configuration *)
type t

(** Prints specs of a partitioning *)
val print : t -> unit

(** [confcost c] gives the reward associated to config [c], based on
    miscellaneous parameters such as number of flights *)
val conf_reward : t -> float

(** [produce t] returns all feasible configurations from configuration [t] *)
val produce : t -> t list

(** [terminal t] asserts whether a configuration [t] is the last to be
    considered *)
val terminal : t -> bool

(** [make_conf int (Util.Sset.t * Util.Smap.key list) list] create a new
    configuration for a given timestep [t] and partition
    [(Util.Sset.t * Util.Smap.key list) list]*)
val make_root : (Util.Sset.t * Util.Smap.key list) list -> t

(****************************************************************)
(* Only for debug *)
(****************************************************************)
val get_partitions : t -> (Util.Sset.t * Util.Smap.key list) list
val get_time : t -> int
(****************************************************************)
