(** Module to split a scenario file *)

(** [divide f n] divides scenario in file [f] in [n] parts. Outputs are in
    f0, ..., fn *)
val divide : string -> int -> unit
