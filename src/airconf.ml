type partition = (Util.Sset.t * Util.Smap.key list) list

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

let _nsec = 5

let _ctx = Partitions.make_context l

module type Environment = sig
  val tmax : int
  val alpha : float
  val beta : float
  val gamma : float
  val lambda : float
  val theta : float
  val init : partition
  val workload : int -> string list -> float * float * float
end

module type S = sig
  type t
  val init : t
  val print : t -> unit
  val cost : t -> float
  val produce : t -> t list
  val terminal : t -> bool

  (****************************** DEBUG **************************************)
  val get_partitions : t -> partition
  val get_time : t -> int
end

module Make (Env : Environment) = struct

  type t = {
    time : int; (* Used to determine whether the node is terminal *)
    partition : partition;
    cost : float
  }

  type estim_load =
    | Low
    | Normal
    | High

  module type Memoize = sig
    type key
    type element
    val add : key -> element -> unit
    val find : key -> element
    val mem : key -> bool
  end

  module PartitionTools = struct
    type s = partition
    type d = s list
    let length = 25
    let normalise =
      List.sort (fun p1 p2 -> compare (List.hd @@ snd p1) (List.hd @@ snd p2))
  end

  module StatusTools = struct
    type s = t
    type d = s list
    let length = 500
    let normalise c =
      { c with
        partition = List.sort (fun p1 p2 ->
            compare (List.hd @@ snd p1) (List.hd @@ snd p2)) c.partition }
  end

  module PartMem = Memoize.Make(PartitionTools)
  module StatMem = Memoize.Make(StatusTools)

  let print s = Printf.printf "time/length/cost:\t" ;
    Printf.printf "%d/%d/%f" s.time (List.length s.partition)
      s.cost ;
    print_newline ()

  let e_wl a b c =
    if a > b && a > c then High else if b > a && b > c then Normal else Low

  let workload_costs time part =
    List.fold_left (fun accu sec ->
        let (a, b, c) = accu in
        let (ph, pn, pl) = Env.workload time (Util.Sset.elements (fst sec)) in
        let status = e_wl ph pn pl in
        let card = Util.Sset.cardinal (fst sec) in
        match status with
        | High -> (a +. ph *. (float card) ** 2., b, c)
        | Normal -> (a, b +. pn *. (float card) ** (-2.), c)
        | Low -> (a, b, c +. pl *. (float card) ** (-2.))
      ) (0., 0., 0.) part

  let trans_cost p_father p_child =
    if p_father = p_child then 0. else 1.

  let compute_cost time part p_father =
    let highcost, normalcost, lowcost = workload_costs time part
    and transcost = trans_cost p_father part
    and sizefac = 1. /. (float @@ (_nsec - List.length part)) in
    (
      Env.alpha *. highcost +. Env.beta *. normalcost *. Env.gamma *. lowcost +.
      Env.lambda *. sizefac +.
      Env.theta *. transcost
    ) /. float Env.tmax

  let cost conf = conf.cost

  (* Partition production, i.e. generation of children partitions *)
  let prod_parts_nomem part = part :: Partitions.recombine _ctx part

  (* [prod_parts p] generates all reachable partitions from partition [p].
   * Uses memoization, see nomem version for the original function *)
  let prod_parts part =
    if PartMem.mem part then PartMem.find part else
      let new_parts = prod_parts_nomem part in
      PartMem.add part new_parts ;
      new_parts

  (* [produce c] produces all children states of config *)
      (* No mem version here *)
  let produce config =
    let reachable_partitions = prod_parts_nomem config.partition in
    List.map (fun p ->
        { partition = p ; time = config.time + 1 ;
          cost = compute_cost (config.time + 1) p config.partition }
      ) reachable_partitions

  (* Memoized version of the above *)
  (* let produce config =
    if StatMem.mem config
    then StatMem.find config
    else
      let newconfs = produce_nomem config in
      StatMem.add config newconfs ;
      newconfs *)


  let terminal conf = conf.time >= Env.tmax

  let init =
    { time = 0 ; partition = Env.init ;
      cost = compute_cost 0 Env.init Env.init }

  (*********************** DEBUG *********************************************)
  let get_partitions conf = conf.partition
  let get_time conf = conf.time
end
