(** Functorial interface for mcts *)

(** Input signature of the functor *)
module type Support = sig
  (** State of nodes *)
  type t

  (** The initial state *)
  val init : t

  (** [produce s] returns new states reachable from [s] *)
  val produce : t -> t list

  (** [cost s] returns the cost of a state *)
  val cost : t -> float

  (** [terminal s] returns whether state [s] is terminal *)
  val terminal : t -> bool

  val print : t -> unit
end

(** Signature of second input module, contains parameters of the mcts *)
module type MctsParameters = sig

  (** Tradeoff between exploration and exploitation. The higher the value,
      the more not visited nodes are considered promising. See bandit
      algorithms for more information *)
  val expvexp : float
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
  val best_path_max : tree -> float -> tree list

  (** Selects nodes which have the highest visit count *)
  val best_path_secure : tree -> float -> tree list

  (** Selects nodes which minimise a lower confidence bound *)
  val best_path_robust : tree -> float -> tree list

  (********************************* DEBUG ***********************************)
  val get_state : tree -> state
end

module Make : functor (Supp : Support) (MctsParam : MctsParameters) ->
  S with type state = Supp.t
