(** Functorial interface for mcts *)

(** Input signature of the functor *)
module type SuppS = sig
  (** State of nodes *)
  type t

  (** [produce s] returns new states reachable from [s] *)
  val produce : t -> t list

  (** [conf_cost s] returns the cost of a state *)
  val reward : t -> float

  (** [terminal s] returns whether state [s] is terminal *)
  val terminal : t -> bool

  val print : t -> unit

  val make_root : (Util.Sset.t * Util.Smap.key list) list -> t
end

(** Output signature of the functor *)
module type S = sig

  (** Type of the state embedded in each node *)
  type state

  (** Tree type to explore. Genericity is relevant for functor only *)
  type tree

  (** [best_path_max t n] @returns a path of nodes from the root of [t] to a
   *       terminal leaf. The selection is based on the reward in each node. *)
  val best_path_max : tree -> int -> tree list

  (** Selects nodes which have the highest visit count *)
  val best_path_secure : tree -> int -> tree list

  (** Selects nodes which minimise a lower confidence bound *)
  val best_path_robust : tree -> int -> tree list

  (** create a new root *)
  val make_node : state -> tree

  (********************************* DEBUG ***********************************)
  val get_state : tree -> state
end

module Make : functor (Support : SuppS) -> S with type state = Support.t
