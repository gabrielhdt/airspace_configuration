(* max nb of aircrft in a controlled sector *)
let _threshold = 10.

let l =
    [ ("s1",["1"]);
      ("s2",["2"]);
      ("s3",["3"]);
      ("s4",["4"]);
      ("s5",["5"]);
      ("a",["2";"3"]);
      ("b",["3";"4"]);
      ("c",["4";"5"]);
      ("d",["1";"5"]);
      ("e",["1";"2";"3"]);
      ("f",["1";"2";"3";"4";"5"]);
      ("g",["1";"2"])  ]

let _st_balance = (10., 0.)

let _nmax = 6

let _ctx = Partitions.make_context l

let _le = ["1";"2";"3";"4";"5"];;
let _lp = Partitions.partitions _ctx (fun _-> true) _le;;
let _reachable_partitions = Hashtbl.create 100;;

let sort_partitions_list l =
  List.sort (fun p1 p2 ->
      let (_, label1) = p1 in
      let (_, label2) = p2 in
      compare (List.hd label1) (List.hd label2)
    ) l

(* Printf.printf "length : %d\n" (Hashtbl.length _reachable_partitions);; *)

module type WLS = sig
  val tmax : int
  val f : int -> string -> int
end

module type S = sig
  type t

  val print : t -> unit

  val reward : t -> float

  val produce : t -> t list

  val terminal : t -> bool

  val make_root : (Util.Sset.t * Util.Smap.key list) list -> t

  (****************************** DEBUG **************************************)
  val get_partitions : t -> (Util.Sset.t * Util.Smap.key list) list
  val get_time : t -> int
end

module Make (Workload : WLS) = struct

  type t = {
    time : int ; (* Used to determine whether the node is terminal *)
    partition : (Util.Sset.t * Util.Smap.key list) list;
    transition_cost : float;
    configuration_cost : float
  }

  let print s = Printf.printf "time/length/trc/sc: " ;
    Printf.printf "%d/%d/%f/%f" s.time (List.length s.partition)
      s.transition_cost s.configuration_cost ;
    print_newline ()

  let partition_cost time partition f =
    let stat_cost =
      List.fold_left (fun accu elt ->
          let subset = Util.Sset.elements (fst elt) in
          let current_cost = (* Accumulation of the cost of each control sector *)
            List.fold_left (fun acc m -> acc +. float (f time m)) 0. subset in
          accu +. (if current_cost -. _threshold < 0. then 0.
                   else current_cost -. _threshold)
        )
        0. partition
    in stat_cost +. 0.1 *. (1. +. stat_cost) *. float (List.length partition)

  let trans_cost p_father p_child =
    if p_father = p_child then 0. else 1.

  let produce config =
    (* let reachable_partitions = config.partition ::
                               (Partitions.recombine _ctx config.partition) in *)
    let sorted_part = sort_partitions_list config.partition in
    let reachable_partitions =
      if not (Hashtbl.mem _reachable_partitions sorted_part)
      then
        let children = sorted_part::(Partitions.recombine _ctx sorted_part) in
        (Hashtbl.add _reachable_partitions sorted_part children );
        children
      else Hashtbl.find _reachable_partitions sorted_part in
    (* let reachable_partitions = Hashtbl.find _reachable_partitions config.partition in *)
    List.map (fun p ->
        let cc = partition_cost (config.time + 1) p Workload.f in
        let tc = trans_cost config.partition p in
        {time = (config.time + 1);
         partition = p;
         transition_cost = tc;
         configuration_cost = cc}
      ) reachable_partitions

  let reward conf = 1. /. (
      1. +.
      (fst _st_balance *. conf.configuration_cost +.
       snd _st_balance *. conf.transition_cost)
    )

  let terminal conf = conf.time > Workload.tmax

  let make_root p0 =
    let partition_cost = partition_cost 0 p0 Workload.f in
    {time = 0; partition = p0; transition_cost =0.;
     configuration_cost = partition_cost }

  (*********************** DEBUG *********************************************)
  let get_partitions conf = conf.partition
  let get_time conf = conf.time
end
