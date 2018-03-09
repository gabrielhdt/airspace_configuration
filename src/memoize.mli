(** Ad-hoc functorial memory to speed up computing *)

(** Output type of the functor *)
module type S = sig

  (** Identifies data *)
  type key

  (** Type of the elements *)
  type element = key list

  (** [add k e] adds elemet [e] to memory with key [k] *)
  val add : key -> element -> unit

  (** [find k e] returns element [e] of memory identified by key [k] *)
  val find : key -> element

  (** [mem k] asserts whether an element in memory has the key [k] *)
  val mem : key -> bool
end

(** Argument of the functor, tools to manipulate a data type *)
module type DataTools = sig

  (** Type of the data *)
  type d

  (** Supposed length of the table *)
  val length : int

  (** [normalise d] returns a modified form of data [d] to avoid having
      several forms carrying the same data *)
  val normalise : d -> d
end

module Make : functor (DT : DataTools) -> S with type key = DT.d
