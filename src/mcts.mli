(** Functorial interface for mcts *)

(** Input signature of the functor *)
module type Support = sig
  (** State of nodes *)
  type t

  (** The initial state *)
  val init : t

  (** [produce s] returns new states reachable from [s] *)
  val produce : t -> t list

  (** [conf_cost s] returns the cost of a state *)
  val reward : t -> float

  (** [terminal s] returns whether state [s] is terminal *)
  val terminal : t -> bool

  val print : t -> unit
end

(** Output signature of the functor *)
module type S = sig

  (** Type of the state embedded in each node *)
  type state

  (** Tree type to explore *)
  type tree

  (** The root of the tree *)
  val root : tree

  (** [best_path_max t n] returns a path of nodes from the root of [t] to a
      terminal leaf. The selection is based on the reward in each node. *)
  val best_path_max : tree -> int -> tree list

  (** Selects nodes which have the highest visit count *)
  val best_path_secure : tree -> int -> tree list

  (** Selects nodes which minimise a lower confidence bound *)
  val best_path_robust : tree -> int -> tree list

  (********************************* DEBUG ***********************************)
  val get_state : tree -> state
end

module Make : functor (Supp : Support) -> S with type state = Supp.t
