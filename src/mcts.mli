(** Tree type to explore. Genericity is relevant for functor only *)
type 'a tree

(** [mcts r] updates tree of root [t] with monte carlo *)
val mcts : Airconf.t tree -> unit
