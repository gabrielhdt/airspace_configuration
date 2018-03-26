Arg.parse Options.speclist Options.anon_fun (Options.usage Sys.argv.(0)) ;;

let sc = Scenario.load !Options.scpath
let s15 = Util.Sset.add "1" (Util.Sset.add "5" Util.Sset.empty)
let s32 = Util.Sset.add "3" (Util.Sset.add "2" Util.Sset.empty)
let s4 = Util.Sset.add "4" Util.Sset.empty
let initial_partition = [
  (s15, [("d") ]) ; (s32, [("a") ]) ; (s4, [("s4") ])
]

module Env = struct
  let tmax = !Options.maxsearch
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

let _size = int_of_float @@ 6. ** (float Env.tmax)

let reltable : (int, int list) Hashtbl.t = Hashtbl.create _size
let costtable : (int, float) Hashtbl.t = Hashtbl.create _size

let build_tree root =
  let rec loop st id =
    let cid = succ id in
    if Support.terminal st then cid, Leaf (cid, st)
    else
      let mrid, children = List.fold_left (fun (lid, sib) elt ->
          let nid, nnode = loop elt lid in
          nid, nnode :: sib) (cid, []) (Support.produce st)
      in
      mrid, (Node (cid, st, children))
  in snd @@ loop root ~-1

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

let rec fillcost = function
  | Leaf (id, st) -> Hashtbl.add costtable id (Support.cost st)
  | Node (id, st, children) -> Hashtbl.add costtable id (Support.cost st) ;
      List.iter (fun c -> fillcost c) children

let rec fillrel = function
  | Leaf (id, _) -> Hashtbl.add reltable id []
  | Node (id, _, children) -> let cids = List.map (fun elt ->
      match elt with Leaf (id, _) | Node (id, _, _) -> id) children in
      Hashtbl.add reltable id cids ;
      List.iter fillrel children

let build_graph () =
  Hashtbl.fold (fun key elt acc ->
      let edges = List.map (fun id -> id, Hashtbl.find costtable id) elt in
      IMap.add key edges acc) reltable IMap.empty

let build_graph costs relations =
  IMap.fold (fun id neigh gr ->
      let edges = List.map (fun nid -> nid, IMap.find nid costs) neigh in
      IMap.add id edges gr) IMap.empty IMap.empty

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
  let tree = build_tree Support.init in
  fillrel tree ; fillcost tree ;
  let relations = relmap tree
  and costs = costmap tree in
  let graph = build_graph costs relations in
  print_endline "Graph built, seeking paths..." ;
  let sol = dijkstra graph 0 in
  IMap.iter (fun id { dist = d ; prev = _ } ->
      Printf.printf "Node %d: %f\n" id d) sol
