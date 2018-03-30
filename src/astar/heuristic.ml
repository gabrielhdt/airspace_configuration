module type Environment = sig
  val tmax : int
  val alpha : float
  val beta : float
  val gamma : float
  val lambda : float
  val theta : float
  val init : Partitions.partition
  val workload : int -> string list -> float * float * float
  val ctx : Partitions.context
  val sectors : string list
end

module type Support = sig
  type t
  val init : t
  val produce : t -> t list
  val cost : t -> float
  val terminal : t -> bool
  val print : t -> unit
  val part_cost : int -> Partitions.partition -> float
end

module type S = sig
  val h : int -> float
end

module Make (Env : Environment)(AirSupp : Support) = struct

  let allparts = Partitions.partitions Env.ctx (fun _ -> true) Env.sectors;;

  Printf.printf "nb allparts : %d\n%!" (List.length allparts)

  let best time = Auxfct.argmax (fun p1 p2 ->
      if AirSupp.part_cost time p1 < AirSupp.part_cost time p2 then p1 else p2)
      allparts

  (* [bestof_parts h] builds list of best partitions for each time step *)
  let bestof_parts horizon =
    let rec loop acc cnt = (* [cnt] counts time step *)
      if cnt >= horizon then acc else loop (best cnt :: acc) (cnt + 1) in
    loop [] 0

  (* Path of optimal partitions (one per time step *)
  let bestparts = bestof_parts Env.tmax

  let h time =
    let rec loop b =
      if b >= Env.tmax then 0. else
        (AirSupp.part_cost b (List.nth bestparts b)) +. loop (b + 1) in
    loop time
end
