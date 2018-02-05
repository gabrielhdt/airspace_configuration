(** Utilities to manipulate the airspace configuration *)

(** Specification of a configuration *)
type t

(** A default state *)
val dummy : t

(** [confcost t] gives the forecasted cost of config [t], allows one to
    compare confs *)
val confcost : t -> float

(** [produce t] returns all feasible configurations from configuration [t] *)
val produce : t -> t list
