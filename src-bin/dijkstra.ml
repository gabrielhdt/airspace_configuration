Arg.parse Options.speclist Options.anon_fun (Options.usage Sys.argv.(0)) ;;

let sc = Scenario.load !Options.scpath
let s15 = Util.Sset.add "1" (Util.Sset.add "5" Util.Sset.empty)
let s32 = Util.Sset.add "3" (Util.Sset.add "2" Util.Sset.empty)
let s4 = Util.Sset.add "4" Util.Sset.empty
let initial_partition = [
  (s15, [("d") ]) ; (s32, [("a") ]) ; (s4, [("s4") ])
]

module Env = struct
  let tmax = !Options.horizon
  let alpha = !Options.alpha
  let beta = !Options.beta
  let gamma = !Options.gamma
  let lambda = !Options.lambda
  let theta = !Options.theta
  let init = initial_partition
  let workload = Scenario.workload sc
end

module Support = Airconf.Make(Env)

module IMap = Map.Make(struct type t = int let compare = compare end)
module ISet = Set.Make(struct type t = int let compare = compare end)

(* Mind the init_data default label value! *)
type node_data = { dist : float ; prev : int option }

type status = Support.t
type tree = Node of int * status * tree list
          | Leaf of int * status

let build_tree root =
  let rec loop st id leaves =
    let cid = succ id in
    if Support.terminal st then cid, ISet.add cid leaves, Leaf (cid, st)
    else
      let mrid, mrls, children = List.fold_left (fun (lid, lls, sib) elt ->
          let nid, nls, nnode = loop elt lid lls in
          nid, nls, nnode :: sib) (cid, leaves, []) (Support.produce st)
      in
      mrid, mrls, (Node (cid, st, children))
  in let _, l, t = loop root ~-1 ISet.empty in
  t, l

let rec fold_tree f acc tree = match tree
  with Leaf (id, st) as l -> f acc l
     | Node (id, st, children) as n -> let aacc = f acc n
         in List.fold_left f aacc children

let rec costmap tree =
  let rec loop acc node = match node with
    | Leaf (id, st) -> IMap.add id (Support.cost st) acc
    | Node (id, st, children) -> let amap = IMap.add id (Support.cost st) acc
        in List.fold_left loop amap children
  in loop IMap.empty tree

let rec relmap tree =
  let rec loop acc node = match node
    with Leaf (id, _) -> IMap.add id [] acc
       | Node (id, _, children) -> let cids = List.map (fun elt ->
           match elt with Leaf (id, _) | Node (id, _, _) -> id) children in
           let amap = IMap.add id cids acc in
           List.fold_left loop amap children
  in loop IMap.empty tree

let rec statemap accmap node = match node
  with Leaf (id, st) -> IMap.add id st accmap
     | Node (id, st, children) -> let nmap = IMap.add id st accmap in
       List.fold_left statemap nmap children

let build_graph costs relations =
  IMap.fold (fun id neigh gr ->
      let edges = List.map (fun nid -> nid, IMap.find nid costs) neigh in
      IMap.add id edges gr) relations IMap.empty

let dijkstra (graph : (int * float) list IMap.t) sid =
  let initdata = IMap.fold (fun id _ ndmap ->
      IMap.add id { dist = infinity ; prev = None } ndmap) graph IMap.empty in
  let almostready = IMap.remove sid initdata in
  let ndata = IMap.add sid { dist = 0. ; prev = None } almostready
  and notvisited = IMap.fold (fun id _ nvset -> ISet.add id nvset) graph
      ISet.empty in
  let rec loop nvset dmap =
    if nvset = ISet.empty then dmap else
      let tmpid = ISet.choose nvset in
      let tmpdist = (IMap.find tmpid dmap).dist in
      let mid = fst @@ ISet.fold (fun id acc ->
          let d = (IMap.find id dmap).dist in if d < snd acc then id, d else
            acc) nvset (tmpid , tmpdist) in
      let reducednvset = ISet.remove mid nvset in
      let newdmap = List.fold_left (fun acc (id, cost) ->
          let alt = (IMap.find mid acc).dist +. cost
          and distneigh = (IMap.find id acc).dist in
          if alt < distneigh then
            let newdata = { dist = alt ; prev = Some mid } in
            let racc = IMap.remove id acc in IMap.add id newdata racc
          else acc) dmap (IMap.find mid graph) in
      loop reducednvset newdmap in
  loop notvisited ndata

let () =
  let tree, leaves = build_tree Support.init in
  let relations = relmap tree
  and costs = costmap tree in
  let graph = build_graph costs relations in
  print_endline "Graph built, seeking paths..." ;
  let sol = dijkstra graph 0 in
  let leavesol = IMap.filter (fun id _ -> ISet.mem id leaves) sol in
  let shortest = IMap.fold (fun key nd acc ->
      if nd.dist < acc.dist then nd else acc) leavesol
      { dist = infinity ; prev = None } in
      let smap = statemap IMap.empty tree in
      let rec build_path { dist = _ ; prev = pid } =
        match pid
        with None -> []
           | Some i -> i :: build_path (IMap.find i sol)
      in let path = List.rev @@ build_path shortest in
      let p = List.map (fun id -> IMap.find id smap) path in
      let pathcost = List.fold_left (fun acc elt ->
          acc +. (Support.cost elt)) 0. p in
      let n = List.length p in
      Printf.printf "path cost : %f path length %d \n" (pathcost) n;

      if !Options.verbose then
        Partitions.print_partitions (List.map (fun s ->
            Support.get_partitions s) (List.map (fun tree -> tree) p) )
