(** Functorial interface to manipulate airspace configuration. Takes as input
    a traffic forecasting module *)

(** A type alias, for convenience *)
type partition = (Util.Sset.t * Util.Smap.key list) list

(** Type of the traffic forecasting module *)
module type Environment = sig

  (** Horizon of the forecast, will determine the depth of the tree *)
  val tmax : int

  val alpha : float
  val beta : float
  val gamma : float
  val lambda : float
  val theta : float

  (** Initial partition of the airspace *)
  val init : partition

  (** [workload t m] returns a forecasted load for control sector [m] at
      time [t] *)
  val workload : int -> string list -> float * float * float
end

(** Output signature f the functor *)
module type S = sig
  (** Specification of a configuration *)
  type t

  (** Initial state *)
  val init : t

  (** Prints specs of a partitioning *)
  val print : t -> unit

  (** [reward c] gives the reward associated to config [c], based on
      miscellaneous parameters such as number of flights *)
  val cost : t -> float

(** [produce t] returns all feasible configurations from configuration [t]
    Two versions are available in source code: with or without memoization *)
  val produce : t -> t list

  (** [terminal t] asserts whether a configuration [t] is the last to be
      considered *)
  val terminal : t -> bool

  (****************************** DEBUG **************************************)
  val get_partitions : t -> (Util.Sset.t * Util.Smap.key list) list
  val get_time : t -> int
end

module Make : functor (Env : Environment) -> S
