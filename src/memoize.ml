module type S = sig
  type key
  type element
  val add : key -> element -> unit
  val find : key -> element
  val mem : key -> bool
  val print : unit -> unit
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

  let print () =
    let ts = Hashtbl.stats table in
    Printf.printf "length/number of buckets/max bucket length: " ;
    Printf.printf "%d/%d/%d" ts.num_bindings ts.num_buckets ts.max_bucket_length
end
