module type S = sig
  type key
  type element
  val add : key -> element -> unit
  val find : key -> element
  val mem : key -> bool
end

module type DataTools = sig
  type s
  type d
  val length : int
  val normalise : s -> s
end

module Make (DT : DataTools) = struct
  type key = DT.s
  type element = DT.d
  type t = (key, element) Hashtbl.t

  let (table : t) = Hashtbl.create DT.length

  let add k e = let nelt = DT.normalise k in Hashtbl.add table nelt e

  let find k = Hashtbl.find table (DT.normalise k)

  let mem k = Hashtbl.mem table (DT.normalise k)

end
