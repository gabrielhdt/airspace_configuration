type element = string
type group = string
type subset = Util.Sset.t
type compatible_groups = group list
type partition = (subset * compatible_groups) list
type context = {
  gmap : Util.Sset.t Util.Smap.t;
  emap : Util.Sset.t Util.Smap.t;
}
val member :
  Util.Sset.t Util.Smap.t -> Util.Sset.elt -> Util.Smap.key -> bool
val make_emap : Util.Sset.t Util.Smap.t -> Util.Sset.t Util.Smap.t
val make_context : (Util.Smap.key * Util.Sset.elt list) list -> context
val update_context : context -> Util.Sset.t -> context
val list2string : string list -> string
val set2string : Util.Sset.t -> Util.Sset.elt
val print_map : Util.Sset.t Util.Smap.t -> unit
val print_partition : (Util.Sset.t * string list) list -> unit
val print_partitions : (Util.Sset.t * string list) list list -> unit
exception Not_valid
val possible_groups : context -> Util.Sset.t -> Util.Sset.t
val compatible_groups :
  context -> Util.Sset.t -> Util.Sset.t -> Util.Smap.key list
val update_compatible_groups :
  context ->
  Util.Sset.elt ->
  ('a * Util.Smap.key list) list -> ('a * Util.Smap.key list) list
val add_element :
  context ->
  ((Util.Sset.t * Util.Smap.key list) list -> bool) ->
  Util.Sset.elt ->
  (Util.Sset.t * Util.Smap.key list) list ->
  ((Util.Sset.t * Util.Smap.key list) list -> 'a -> 'a) -> 'a -> 'a
val partitions :
  context ->
  ((Util.Sset.t * Util.Smap.key list) list -> bool) ->
  Util.Sset.elt list -> (Util.Sset.t * Util.Smap.key list) list list
val count :
  context ->
  ((Util.Sset.t * Util.Smap.key list) list -> bool) ->
  Util.Sset.elt list -> int
val recombine2 :
  context ->
  int option ->
  Util.Sset.t -> Util.Sset.t -> (Util.Sset.t * Util.Smap.key list) list list
val transfer :
  context ->
  Util.Sset.t -> Util.Sset.t -> (Util.Sset.t * Util.Smap.key list) list list
val split2 :
  context -> Util.Sset.t -> (Util.Sset.t * Util.Smap.key list) list list
val merge :
  context ->
  Util.Sset.t -> Util.Sset.t -> (Util.Sset.t * Util.Sset.elt list) list list
val recombine :
  context ->
  (Util.Sset.t * Util.Smap.key list) list ->
  (Util.Sset.t * Util.Smap.key list) list list
