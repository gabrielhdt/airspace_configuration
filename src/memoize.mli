(** Ad-hoc functorial memory to speed up computing in {!module:Airconf} *)

(** Output type of the functor *)
module type S = sig

  (** Identifies data *)
  type key

  (** Type of the elements *)
  type element

  (** [add k e] adds elemet [e] to memory with key [k] *)
  val add : key -> element -> unit

  (** [find k e] returns element [e] of memory identified by key [k] *)
  val find : key -> element

  (** [mem k] asserts whether an element in memory has the key [k] *)
  val mem : key -> bool
end

(** Argument of the functor, tools to manipulate a data type *)
module type DataTools = sig

  (** Type of sources of data *)
  type s

  (** Type of the data *)
  type d

  (** Supposed length of the table *)
  val length : int

  (** [normalise s] returns a modified form of source [s] to avoid having
      several sources carrying the same data *)
  val normalise : s -> s
end

module Make : functor (DT : DataTools) -> S
  with type key = DT.s with type element = DT.d
