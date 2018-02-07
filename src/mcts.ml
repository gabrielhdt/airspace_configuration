type 'a tree = {
  state : 'a ;
  mutable q : float ;
  mutable n : int ;
  mutable children : 'a tree list
}

(* Contains ways to select final best path *)
module Win_select = struct

  (* arbitrary cste given by chaslot *)
  let _a = 4.

  (* define le low confidence bound *)
  let lcb node =
    node.q +. _a /. sqrt (float node.n)

  (* some functions that define how to select the final path
    (best q, best n of best lcb)*)
  let cmp_max n1 n2 =
    if n1.q < n2.q then n2 else n1

  let cmp_robust n1 n2 =
  if n1.n < n2.n then n2 else n1

  let cmp_secure n1 n2 =
    let lcb1 = lcb n1 and lcb2 = lcb n2 in
    if lcb1 < lcb2 then n2 else n1

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

let nsim = 100

(* Temporary considering functor approach *)
let dummystate = Airconf.dummy

let beta = 1.

let random_elt lst =
  let i = Random.int (List.length lst) in
  List.nth lst i

(* [produce t] creates the list of reachable nodes from [t] *)
let produce t =
  let states = Airconf.produce t.state in
  List.map (fun s -> { state = s ; q = 0. ; n = 0 ; children = [] }) states

(** [force_deploy t] tries to add children from a production rule *)
let force_deploy t =
  match t.children with
  | [] -> t.children <- produce t
  | _ :: _ -> ()

(* TODO The two functions below could be grouped in one which would expand the
 * node or raise an exception *)
let expandable t =
  (* Parses the children to see if one has not been visited *)
  let rec loop = function
    | [] -> false
    | hd :: tl -> if hd.n = 0 then true else loop tl
  in
  if Airconf.terminal t.state then false else loop t.children

(** [expand t] returns node which must be visited among children of [t] *)
let expand t =
  let rec loop = function
    | [] -> failwith "no nodes to expand"
    | hd :: tl ->
        if hd.n = 0 then hd else loop tl
  in
  loop t.children

let ucb beta father child =
  child.q /. float child.n +.
  beta *. sqrt (2. *. log (float father.n) /. (float child.n))

(* Could be speeded up, avoid recomputing ucb of best node if it hasn't
 * changed *)
let best_child t =
  let aux ta tb =
    let va = ucb beta t ta
    and vb = ucb beta t tb
    in
    if va >= vb then ta else tb
  in
  List.fold_left aux (List.hd t.children) t.children

(** [select t a] builds a path toward most urgent node to expand *)
    (* TODO see above comment considering expand *)
let rec select tree ancestors =
  force_deploy tree ;
  if expandable tree then tree :: ancestors
  else
    match tree.children with
    | (hd :: tl) -> let favourite = best_child tree in
        select favourite (tree :: ancestors)
    | [] -> ancestors

let treepolicy root =
  let path = select root [] in
  let exnode = expand (List.hd path) in
  exnode :: path

(** [simulate t] parses the tree [t] randomly until a terminal state is found,
    and returnsan evaluation of the path *)
let simulate t =
  let rec loop lt acc =
    let cost = Airconf.confcost lt.state in
    if Airconf.terminal lt.state then cost +. acc
    else
      let children = produce lt in
      let randchild = random_elt children in
      loop randchild (acc +. cost)
  in
  loop t 0.

(** [defaultpolicy n] gives a list of the result of [nsim] simulations *)
let defaultpolicy tree =
  let rec loop cnt acc =
    if cnt > nsim then acc
    else loop (cnt + 1) (simulate tree :: acc)
  in
  loop 0 []

(** [backpropagate a w] updates ancestors [a] with the result win [w] *)
let rec backpropagate (ancestors : 'a tree list) reward =
  match ancestors with
  | [] -> ()
  | hd :: tl ->
      begin
        hd.q <- hd.q +. reward ;
        hd.n <- succ hd.n ;
        backpropagate tl reward
      end

(** [mcts r] updates tree of root [t] with monte carlo *)
let mcts root =
  for i = 1 to 4 do
    let path = treepolicy root in
    let wins = defaultpolicy (List.hd path) in
    let bppg_aux win = backpropagate path win in
    List.iter bppg_aux wins
  done


let best_path root criterion =
  let rec aux current_node accu =
    if current_node.children <> [] then accu
    else
      let best_ch = criterion current_node.children in
      (aux best_ch (best_ch::accu) )
  in
  aux root [root]
