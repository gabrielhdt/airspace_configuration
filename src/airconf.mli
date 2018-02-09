(** Utilities to manipulate the airspace configuration *)

(** Specification of a configuration *)
type t

(** [confcost t ( int -> string -> int ) ] gives the actual cost of config [t],
    according to all the flights in the sector. allows one to
    compare configurations *)
val conf_cost : t -> ( int -> string -> int ) -> float

(** [confcost t -> t ] gives the cost of transition [t -> t],
    allows one to compare transitions *)
val trans_cost : t -> t -> float

(** [path_cost ( int -> string -> int ) t list ( t -> ( int -> string -> int ) -> float )
    ( t -> t -> float )] gives the cost of a given path [t list] for all the
    flights in the sector at each timestep*)
val path_cost : ( int -> string -> int ) -> t list ->
  ( t -> ( int -> string -> int ) -> float ) ->
  ( t -> t -> float ) -> float

(** [produce t] returns all feasible configurations from configuration [t] *)
val produce : t -> t list

(** [terminal t] asserts whether a configuration [t] is the last to be
    considered *)
val terminal : t -> bool

(** [make_conf int (Util.Sset.t * Util.Smap.key list) list] create a new
    configuration for a given timestep [t] and partition
    [(Util.Sset.t * Util.Smap.key list) list]*)
val make_conf : int -> (Util.Sset.t * Util.Smap.key list) list -> t
