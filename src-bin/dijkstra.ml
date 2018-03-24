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

(* Mind the init_data default label value! *)
type cost = float
type node_aux = {id: int ; mutable label: cost ; mutable father: int}
type node = {id: int ; edges: (int * cost) list}
type graph = node array

type status = Support.t
type tree = Node of int * status * tree list
          | Leaf of int * status

let reltable : (int, int list) Hashtbl.t = Hashtbl.create 10000
let costtable : (int, cost) Hashtbl.t = Hashtbl.create 10000

let build_tree root produce term =
  let rec loop st id =
    let cid = succ id in
    if term st then cid, Leaf (cid, st)
    else
      let mrid, children = List.fold_left (fun (lid, sib) elt ->
          let nid, nnode = loop elt lid in
          nid, nnode :: sib) (cid, []) (produce st)
      in
      mrid, (Node (cid, st, children))
  in loop root 0

let rec fillcost cost node = match node with
  | Leaf (id, st) -> Hashtbl.add costtable id (cost st)
  | Node (id, st, children) -> Hashtbl.add costtable id (cost st) ;
      List.iter (fun c -> fillcost cost c) children

let rec fillrel = function
  | Leaf (id, st) -> ()
  | Node (id, _, children) -> let cids = List.map (fun elt ->
      match elt with Leaf (id, _) | Node (id, _, _) -> id) children in
      Hashtbl.add reltable id cids

let build_graph () =
  Array.of_list @@ Hashtbl.fold (fun key elt acc ->
      let edges = List.map (fun id -> id, Hashtbl.find costtable id) elt in
      { id = key ; edges = edges } :: acc) reltable []

let print_data_lengths data =
  Printf.printf "[|" ;
  for i=0 to (Array.length data) - 1 do
    Printf.printf "(%f, %d); " data.(i).label data.(i).father
  done ;
  Printf.printf "|]\n"

let update data_orig (piv: node) id_fixed =
  let data = Array.copy data_orig
  in
  let rec loop = function
    | [] -> data
    | hd :: tl ->
        if not (List.mem (fst hd) id_fixed) then (* Not already fixed *)
          let frompiv_cost = data.(piv.id).label +. (snd hd)
          in
          if data.(fst hd).label > frompiv_cost then
            begin
              data.(fst hd).label <- frompiv_cost ;
              data.(fst hd).father <- piv.id
            end ;
          loop tl
        else loop tl
  in
  loop piv.edges

let cmplabel a b = compare a.label b.label

let argmin_notfixed data id_fixed =
  let datalst_tmp = Array.to_list data
  in
  let datalst_s = List.sort cmplabel datalst_tmp in
  let rec extract_notfixed (dataso: node_aux list) = match dataso with
    | [] -> (-1)
    | hd :: tl ->
    if List.mem hd.id id_fixed then extract_notfixed tl
    else hd.id
  in
  extract_notfixed datalst_s

let init_data i x = {id=i ; label=infinity ; father=(-1)}

let dijkstra (graph: node array) source =
  (* Init *)
  let n = Array.length graph in
  let pre_data = Array.make n {id=0 ; label=infinity ; father=(-1)} in
  let data = Array.mapi init_data pre_data
  and id_fixed = [source]
  and piv = graph.(source) in
  data.(source) <- {id=source ; label=0. ; father=(-1)} ;
  (* Loop *)
  let rec loop fixed piv data k =
    if k = n-1 then data else
      let new_data = update data piv fixed
      in
      let piv = graph.(argmin_notfixed new_data fixed)
      in
      loop (piv.id :: fixed) piv new_data (k+1)
  in
  loop id_fixed piv data 0

let () =
  let _, tree = build_tree Support.init Support.produce Support.terminal in
  fillrel tree ; fillcost Support.cost tree ;
  let graph = build_graph () in
  Printf.printf "Graph of %d nodes\n" (Array.length graph) ;
  let sol = dijkstra graph 0
  in
  Printf.printf "Array of (cost, father): " ;
  print_data_lengths sol
