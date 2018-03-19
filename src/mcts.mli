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

  (** Amount of time during which the mcts develops the tree *)
  val lapse : float
end


(** Output signature of the functor *)
module type S = sig

  (** Type of the state embedded in each node *)
  type state

  (** Tree type to explore *)
  type tree

  (** The root of the tree *)
  val root : tree

  (** [mcts t] develops the tree [t] thanks to production rules *)
  val mcts : tree -> unit

  (** [select_max t] returns the child of tree [t] maximising the reward
      expectancy *)
  val select_max : tree -> tree

  (** [select_secure t] returns the child of tree [t] maximising the number
      of visits *)
  val select_robust : tree -> tree

  (** [select_robust t] returns the child of tree [t] maximising a lower
      confidence bound *)
  val select_secure : tree -> tree

  (********************************* DEBUG ***********************************)
  val get_state : tree -> state
end

module Make : functor (Supp : Support) (MctsParam : MctsParameters) ->
  S with type state = Supp.t
