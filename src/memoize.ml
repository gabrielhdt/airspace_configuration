module type S = sig
  type key
  type element = key list
  val add : key -> element -> unit
  val find : key -> element
  val mem : key -> bool
end

module type DataTools = sig
  type d
  val length : int
  val normalise : d -> d
end

module Make (DT : DataTools) = struct
  type key = DT.d
  type element = DT.d list
  type t = (key, element) Hashtbl.t

  let (table : t) = Hashtbl.create DT.length

  let add k e = let nelt = DT.normalise k in Hashtbl.add table nelt e

  let find k = Hashtbl.find table (DT.normalise k)

  let mem k = Hashtbl.mem table (DT.normalise k)

end
