let _beta = 1.

type 'a tree = {
  state : 'a ;
  mutable q : float ;
  mutable n : int ;
  mutable children : 'a tree list
}

let make_node state =
  {
  state = state ;
  q = 0. ;
  n = 0 ;
  children = []
}
(****************************************************************)
(* Only for debug *)
(****************************************************************)
let get_state node = node.state

(****************************************************************)

(* Contains ways to select final best path *)
module Win_pol = struct

  (* arbitrary cste given by chaslot *)
  let _a = 4.

  (* define le low confidence bound *)
  let lcb node =
    node.q +. _a /. sqrt (float node.n +. 1.)

  (* some functions that define how to select the final path
    (best q, best n of best lcb)*)
  let cmp_max n1 n2 =
    if n1.q < n2.q then n2 else n1

  let cmp_robust n1 n2 =
  if n1.n < n2.n then n2 else n1

  let cmp_secure n1 n2 =
    if lcb n1 < lcb n2 then n2 else n1

  (*********)

  (* Select the child with highest reward *)
  let max children =
    Auxfct.argmax cmp_max children

  (* Select the most visited child *)
  let robust children =
    Auxfct.argmax cmp_robust children

  (* Select the child which maximises a lower confidence bound *)
  let secure children =
    Auxfct.argmax cmp_secure children
end

(************************ DEBUG UTIL *******************************)
let print_node node =
  Printf.printf "max/robust/secure scores: " ;
  let lcb = Win_pol.lcb node in
  Printf.printf "%f/%d/%f\n" node.q node.n lcb
(*******************************************************************)

(* Selection policy *)
module Sel_pol = struct
  (* TODO: add single player mcts 3rd term *)
  let ucb beta father child =
    child.q /. float child.n +.
    beta *. sqrt (2. *. log (float father.n) /. (float child.n))

  let best_child t =
    let cmp_ucb ta tb =
      if ucb _beta t ta >= ucb _beta t tb then ta else tb
    in
    Auxfct.argmax cmp_ucb t.children
end

(* [produce t] creates the list of reachable nodes from [t] *)
    (* Functor should create the produce rule *)
let produce t =
  let states = Airconf.produce t.state in
  List.map (fun s -> { state = s ; q = 0. ; n = 0 ; children = [] }) states

(** [force_deploy t] tries to add children from a production rule *)
let force_deploy node =
  match node.children with
  | [] -> node.children <- produce node
  | _ :: _ -> ()

(* TODO The two functions below could be grouped in one which would expand the
 * node or raise an exception *)
let expandable node =
  (* Parses the children to see if one has not been visited *)
  let rec loop = function
    | [] -> false
    | hd :: tl -> if hd.n = 0 then true else loop tl
  in
  if Airconf.terminal node.state then false else loop node.children

(** [expand t] returns node which must be visited among children of [t] *)
let expand node =
  let rec loop = function
    | [] -> failwith "no nodes to expand"
    | hd :: tl -> if hd.n = 0 then hd else loop tl
  in
  loop node.children

(** [select t a] builds a path toward most urgent node to expand *)
    (* TODO see above comment considering expand *)
let rec select node ancestors =
  force_deploy node ;
  if expandable node then node :: ancestors
  else
    match node.children with
    | (hd :: tl) -> let favourite = Sel_pol.best_child node in
      select favourite (node :: ancestors)
    | [] -> ancestors

let treepolicy root =
  let path = select root [] in
  let exnode = expand (List.hd path) in
  exnode :: path

(** [simulate_once t] parses the tree [t] randomly until a terminal state is found,
    and returns an evaluation of the path *)
let simulate_once node =
  let rec loop next_node accu =
    let cost = Airconf.conf_cost next_node.state in
    if Airconf.terminal next_node.state then cost +. accu
    else
      begin
        force_deploy next_node;
        let randchild = Auxfct.random_elt next_node.children in
        loop randchild (accu +. cost)
      end
  in
  loop node 0.

(** [simulate n ns] starting from node [n] gives the results of [ns]
    simulations *)
let simulate node nsim =
  let rec loop cnt acc =
    if cnt > nsim then acc
    else loop (cnt + 1) (simulate_once node :: acc)
  in
  loop 0 []

(** [backpropagate a w] updates ancestors [a] with the result win [w] *)
let rec backpropagate ancestors reward n =
  match ancestors with
  | [] -> ()
  | hd :: tl ->
      begin
        hd.q <- hd.q +. reward;
        hd.n <- hd.n + n ;
        backpropagate tl reward n
      end

(** [mcts r] updates tree of root [t] with monte carlo *)
let mcts root nsim =
  let flag = ref false in
  (* while not !flag do *)
  for i = 0 to 5 do
    let path = treepolicy root in
    let wins = simulate (List.hd path) nsim in
    let reward = List.fold_left (fun accu e -> accu +. e) 0. wins in
    let n = List.length wins in (* should be nsim *)
    backpropagate path reward n;
    flag := Airconf.terminal (get_state (List.hd path));
    Printf.printf "%02d %d\n" (Airconf.get_time (get_state (List.hd path)))
      (List.length path)
  done

let best_path root criterion =
  let rec aux current_node accu =
    match current_node.children with
    | [] -> accu
    | cnc ->
      Printf.printf "------New selection--------\n" ;
      List.iter print_node cnc ;
      let best_ch = criterion cnc in
      (aux best_ch (best_ch::accu) )
  in
  aux root [root]

let best_path_max root nsim =
  mcts root nsim;
  best_path root Win_pol.max

let best_path_secure root nsim =
  mcts root nsim;
  best_path root Win_pol.secure

let best_path_robust root nsim =
  mcts root nsim;
  best_path root Win_pol.robust
