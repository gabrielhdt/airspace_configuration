(* Mind the init_data default label value! *)
type cost = float
type node_aux = {id: int ; mutable label: cost ; mutable father: int}
type node = {id: int ; edges: (int * cost) list}
type graph = node array

type status = unit
type tree = Node of int *  status * tree list
          | Leaf of int * status

let reltable : (int, int list) Hashtbl.t = Hashtbl.create 10000

let build_tree root produce term =
  let rec loop st id =
    let cid = succ id in
    if term st then cid, Leaf (cid, st)
    else
      let mrid, children = List.fold_left ( fun (lid, sib) elt ->
          let nid, nnode = loop elt lid in
          nid, nnode :: sib) (cid, []) (produce st)
      in
      mrid, (Node (cid, st, children))
  in loop root 0

let rec fillrel = function
  | Leaf (id, st) -> ()
  | Node (id, _, children) -> let cids = List.map (fun elt ->
      match elt with Leaf (id, _) | Node (id, _, _) -> id) children in
      Hashtbl.add reltable id cids

let build_graph cost =
  Array.of_list @@ Hashtbl.fold (fun key elt acc ->
      let edges = List.map (fun id -> id, cost id) elt in
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
  let graph = [|
    {id=0 ; edges=[(5, 3.) ; (2, 9.) ; (1, 8.)]} ;
    {id=1 ; edges=[(2, 3.) ; (3, 1.) ; (5, 1.)]} ;
    {id=2 ; edges=[(1, 1.) ; (3, 1.) ; (4, 3.)]} ;
    {id=3 ; edges=[(1, 1.) ; (2, 1.) ; (4, 1.)]} ;
    {id=4 ; edges=[]} ;
    {id=5 ; edges=[(0, 1.) ; (1, 3.) ; (3, 6.) ; (4, 1.)]}
  |]
  in
  let sol = dijkstra graph 0
  in
  Printf.printf "Array of (cost, father): " ;
  print_data_lengths sol
