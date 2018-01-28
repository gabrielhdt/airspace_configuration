(** Utilities to manipulate the airspace configuration *)

(** Specification of a configuration *)
type t

(** [confcost t] gives the forecasted cost of config [t], allows one to
    compare confs *)
val confcost : t -> float
