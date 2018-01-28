type 'a node = {
  tag : int ;
  mutable profit : int ;
  mutable explored : int ;
  children : 'a list
}

type tree =
    Leaf of bool
  | Node of tree node

let dummynode = { tag = 0 ; profit = 0 ; explored = 0 ;
                  children = ([] : tree list) }

let dummytree = Node dummynode

let beta = 1.

(* Allows one to compare two node *)
let nodecmp (na : tree) (nb : tree) =
  let da = match na with
      Leaf x -> 0
    | Node { profit = p ; explored = e ; children = _ } -> p * e
  in
  let db = match nb with
      Leaf x -> 0
    | Node { profit = p ; explored = e ; children = _ } -> p * e
  in
  compare da db

let ucb beta father na nb =
  let aux n = float na.profit /. float na.explored +.
              beta *. sqrt (
                2. *. log (float father.explored) /. (float na.explored))
  in
  let qa = aux na
  and qb = aux nb in
  if qa >= qb then na else nb

let treepolicy (node : tree node) =
  let rec loop rem acc = match rem with
    | [] -> acc
    | hd :: tl ->
        match hd with
        | Leaf _ -> failwith "Too far for treepolicy"
        | Node hdn ->
            if hdn.explored = 0 then hdn
            else ucb beta node hdn acc
  in
  loop node.children dummynode

let argnodecmp nodea nodeb =
  let ncmp = nodecmp nodea nodeb in
  if ncmp >= 0 then nodea
  else nodeb

let random_elt lst =
  let i = Random.int (List.length lst) in
  List.nth lst i

let select (children : tree list) =
  List.fold_left argnodecmp dummytree children

let rec treewalk tree ancestors =
  match tree with
  | Node n ->
      if n.explored = 0 then ancestors
      else let favourite = select n.children in
        treewalk favourite (n :: ancestors)
  | Leaf _ -> ancestors

let rec simulate = function
  | Leaf k -> k
  | Node { profit = _ ; explored = _ ; children = c } ->
      let randchild = random_elt c in
      simulate randchild

(* TODO One propagation for all plays *)
let rec backpropagate ancestors win = match ancestors with
  | [] -> ()
  | hd :: tl -> begin
      hd.explored <- succ hd.explored ;
      if win then hd.profit <- succ hd.profit ;
      backpropagate tl win
    end

let disp_children_stat node =
  List.iter (fun n ->
      match n with Node hd ->
        Printf.printf "Child %d: Q=%d, n=%d\n" hd.tag hd.profit
          hd.explored) node.children

let () =
  Random.self_init () ;
  let t1 = Leaf true
  and t2 = Leaf false
  and t3 = Leaf false
  and t4 = Leaf true
  and t5 = Leaf true
  and t6 = Leaf false in
  let t7 = Node { dummynode with tag = 7 ; children = [ t1 ; t2 ; t3 ] }
  and t8 = Node { dummynode with tag = 8 ; children = [ t4 ; t5 ] }
  and t9 = Node { dummynode with tag = 9 ; children = [ t6 ] } in
  let root = Node { tag = 1 ; profit = 0 ; explored = 0 ;
                    children = [ t7 ; t8 ; t9 ] }
  in
  let rnode = match root with Node n -> n in
  let nodes  = treewalk root [ rnode ] in
  let nsim = 100 in
  let plays = ref [] in
  for i = 1 to nsim do
    plays := simulate (Node (List.hd nodes)) :: !plays
  done ;
  let rec loopprop results = match results with
      [] -> ()
    | hd :: tl -> backpropagate nodes hd ;
        loopprop tl
  in
  loopprop !plays ;
  Printf.printf "Q: %d ; n : %d\n" rnode.profit rnode.explored ;
  disp_children_stat rnode
