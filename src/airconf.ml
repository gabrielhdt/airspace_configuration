type partition = (Util.Sset.t * Util.Smap.key list) list
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

(* Signature of the memory module used to fasten up the production *)
module type ProductionMemory = sig
  (* Those types are exported since working with partition is a secret for
   * no one ... *)
  type key = partition
  type element = partition list

  (* Type of the memory *)
  type t

  val add : key -> element -> unit
  val find : key -> element
  val mem : key -> bool
end

(* Memory of production rule. The memory is in place. Partitions given as key
 * are automatically normalised (control sectors of the partition are sorted)
 * to ensure that the same partition isn't entered twice. *)
module ProdMem : ProductionMemory = struct
  type key = partition
  type element = partition list
  type t = (key, element) Hashtbl.t

  (* TODO find a valid formula for initial length... *)
  let (table : t) = Hashtbl.create (List.length l * 2)

  let normalise_partition p =
    List.sort (fun p1 p2 -> compare (List.hd @@ snd p1) (List.hd @@ snd p2)) p

  (* [add p q] adds partition list [q] with key [p] to the hash table. The key
     is sorted before being added into the table *)
  let add part reachable_partitions =
    let (spart : partition) = normalise_partition part in
    Hashtbl.add table spart reachable_partitions

  let find part = Hashtbl.find table (normalise_partition part)

  let mem part = Hashtbl.mem table (normalise_partition part)
end

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
  val make_root : partition -> t

  (****************************** DEBUG **************************************)
  val get_partitions : t -> partition
  val get_time : t -> int
end

module Make (Workload : WLS) = struct

  type t = {
    time : int; (* Used to determine whether the node is terminal *)
    partition : partition;
    transition_cost : float;
    configuration_cost : float
  }

  module type StatusMemory = sig
    type key = t
    type element = t list

    val table : (key, element) Hashtbl.t
    val add : key -> element -> unit
    val find : key -> element
    val mem : key -> bool
  end

  module StatMem : StatusMemory = struct
    type key = t
    type element = t list
    type t = (key, element) Hashtbl.t

    (* TODO find valid formula to set inital length *)
    let (table : t) = Hashtbl.create 1000

    let normalise_conf c =
      { c with
        partition = List.sort (fun p1 p2 ->
            compare (List.hd @@ snd p1) (List.hd @@ snd p2)) c.partition }

    let find conf = Hashtbl.find table (normalise_conf conf)

    let mem conf = Hashtbl.mem table (normalise_conf conf)

    let add conf children_conf =
      Hashtbl.add table (normalise_conf conf) children_conf
  end

  let print s = Printf.printf "time/length/trc/sc: " ;
    Printf.printf "%d/%d/%f/%f" s.time (List.length s.partition)
      s.transition_cost s.configuration_cost ;
    print_newline ()


  let partition_cost time partition f =
    let stat_cost =
      List.fold_left ( fun accu elt ->
          let subset = Util.Sset.elements (fst elt) in
          let current_cost = (* Accumulation of the cost of each control sector *)
            List.fold_left (fun acc m -> acc +. float ( f time m) ) 0. subset in
          accu +. ( if current_cost -. _threshold < 0. then 0.
                   else current_cost -. _threshold )
        )
        0. partition
    in stat_cost +. 0.1 *. ( 1. +. stat_cost ) *. float (List.length partition)

  let trans_cost p_father p_child =
    if p_father = p_child then 0. else 1.


  (* Partition production, i.e. generation of children partitions *)
  let prod_parts_nomem part = part :: Partitions.recombine _ctx part

  (* [prod_parts p] generates all reachable partitions from partition [p],
     uses memoization *)
  let prod_parts part =
    if ProdMem.mem part then ProdMem.find part else
      let new_parts = prod_parts_nomem part in
      ProdMem.add part new_parts ; new_parts

  (* [produce c] produces all children states of config *)
  let produce_nomem config =
    let reachable_partitions = prod_parts config.partition in
    List.map (fun p ->
        let cc = partition_cost ( config.time + 1 ) p Workload.f in
        let tc = trans_cost config.partition p in
        { time = (config.time + 1);
         partition = p;
         transition_cost = tc;
         configuration_cost = cc }
      ) reachable_partitions

  (* Memoized version of the above *)
  let produce config =
    if StatMem.mem config then StatMem.find config else
      let newconfs = produce_nomem config in
      StatMem.add config newconfs ; newconfs


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
