module type Environment = sig
  val tmax : int
  val alpha : float
  val beta : float
  val gamma : float
  val lambda : float
  val theta : float
  val init : Partitions.partition
  val workload : int -> string list -> float * float * float
  val ctx : Partitions.context
  val sectors : string list
end

module type Support = sig
  type t
  val init : t
  val produce : t -> t list
  val cost : t -> float
  val terminal : t -> bool
  val print : t -> unit
  val part_cost : int -> Partitions.partition -> float
end

module type S = sig
  val h : int -> float
end

module Make :functor (Env : Environment) (AirSupp : Support) -> S
