(** Tree type to explore. Genericity is relevant for functor only *)
type 'a tree

(** [best_path_max t n] @returns a path of nodes from the root of [t] to a
    terminal leaf. The selection is based on the reward in each node. *)
val best_path_max : Airconf.t tree -> int -> Airconf.t tree list

(** Selects nodes which have the highest visit count *)
val best_path_secure : Airconf.t tree -> int -> Airconf.t tree list

(** Selects nodes which minimise a lower confidence bound *)
val best_path_robust : Airconf.t tree -> int -> Airconf.t tree list

val make_node : 'a -> 'a tree
