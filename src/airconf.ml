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

let _cxt= Partitions.make_context l

let _nmax = 15

let sc = Scenario.load "data/scen1.json"

let f = Scenario.f sc

type t = {
  time : int ; (* Used to determine whether the node is terminal *)
  partition : (Util.Sset.t * Util.Smap.key list) list;
  transition_cost : float;
  configuration_cost : float
}

let partition_cost time partition f =
  List.fold_left (fun accu elt ->
    let subset = Util.Sset.elements (fst elt) in
    let current_cost = List.fold_left (fun ac m ->
        ac +. float (f time m)
      ) 0. subset in
    accu +. current_cost /. _threshold
  ) 0. partition

let trans_cost p_father p_child =
  if p_father = p_child then 0. else 10.

let produce config =
  let reachable_partitions = Partitions.recombine _cxt config.partition in
  List.map (fun p ->
      let cc = partition_cost (config.time + 1) p f in
      let tc = trans_cost config.partition p in
      {time = (config.time + 1);
       partition = p;
       transition_cost = cc;
      configuration_cost = tc}
    ) reachable_partitions

let conf_cost conf =
  -. (conf.transition_cost +. conf.configuration_cost)

let terminal conf = conf.time > _nmax

let make_root p0 =
  let partition_cost = partition_cost 0 p0 f in
  {time = 0; partition = p0; transition_cost =0.;
   configuration_cost = partition_cost }

(****************************************************************)
(* Only for debug *)
(****************************************************************)
let get_partitions conf = conf.partition
let get_time conf = conf.time
(****************************************************************)
