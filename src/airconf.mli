(** Utilities to manipulate the airspace configuration *)

(** Specification of a configuration *)
type t

(** A default state *)
val dummy : t

(** [confcost t] gives the actual cost of config [t], allows one to
    compare configurations *)
val confcost : t  -> float

(** [confcost t -> t ] gives the cost of transition [t -> t],
    allows one to compare transitions *)
val tracost : t -> t -> float

val path_cost : t -> ( t -> float ) -> ( t -> t -> float ) -> float

(** [produce t] returns all feasible configurations from configuration [t] *)
val produce : t -> t list

(** [terminal t] asserts whether a configuration [t] is the last to be
    considered *)
val terminal : t -> bool
