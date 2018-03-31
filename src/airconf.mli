(** Functorial interface to manipulate airspace configuration. Takes as input
    a traffic forecasting module *)

(** A type alias, for convenience *)
type partition = Partitions.partition

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

  (** the context associated with the scenario played *)
  val ctx : Partitions.context
  val sectors : string list

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

  (** [h c] gives a minoring path cost from the config to the end of the
      simulation (heuristic used for instance in A* *)
  val h : t -> float

  (** [produce t] returns all feasible configurations from configuration [t]
      Two versions are available in source code: with or without memoization *)
  val produce : t -> t list

  (** [equal s1 s2] asserts whether two states are considered equal *)
  val equal : t -> t -> bool

  (** [terminal t] asserts whether a configuration [t] is the last to be
      considered *)
  val terminal : t -> bool

  (****************************** DEBUG **************************************)
  val get_partitions : t -> partition
  val get_time : t -> int
end

module Make : functor (Env : Environment) -> S
