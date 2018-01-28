type 'a tree = {
  state : 'a ;
  mutable q : float ;
  mutable n : int ;
  children : 'a tree list
}

let nsim = 100

(* Temporary considering functor approach *)
let dummystate = Airconf.dummy

let dummytree = {
  state = dummystate ;
  q = 0. ;
  n = 0 ;
  children = []
}

let beta = 1.

let random_elt lst =
  let i = Random.int (List.length lst) in
  List.nth lst i

(* TODO The two functions below could be grouped in one which would expand the
 * node or raise an exception *)
let rec expandable t =
  if Airconf.terminal t then false else
    (* Parses the children to see if one has not been visited *)
    let rec loop = function
      | [] -> false
      | hd :: tl -> if hd.n = 0 then true else loop tl
    in
    loop t.children

(** [expand t] returns node which must be visited among children of [t] *)
let expand t =
  let rec loop = function
      [] -> failwith "no nodes to expand"
    | hd :: tl ->
        if Airconf.terminal hd || hd.n = 0 then loop tl
        else hd
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
  if expandable tree then ancestors
  else
    match tree.children with
    | (hd :: tl) -> let favourite = best_child tree in
        select favourite (tree :: ancestors)
    | [] -> ancestors

let treepolicy root =
  let path = select root [ root ] in
  let exnode = expand (List.hd path) in
  exnode :: path

(** [simulate t] parses the tree [t] randomly until a terminal state is found,
    and returnsan evaluation of the path *)
let rec simulate t = 
  if Airconf.terminal t then Airconf.confcost t
  else
    let children = Airconf.produce t in
    let randchild = random_elt children in
    simulate randchild

(** [defaultpolicy n] gives a list of the result of [nsim] simulations *)
let defaultpolicy tree =
  let rec loop cnt acc =
    if cnt >= nsim then acc
    else loop (succ cnt) (simulate tree :: acc)
  in
  loop 0 []

(** [backpropagate a w] updates ancestors [a] with the result win [w] *)
let rec backpropagate (ancestors : 'a tree list) reward =
  match ancestors with
  | [] -> ()
  | hd :: tl ->
      begin
        hd.q <- List.hd reward ;
        hd.n <- succ hd.n ;
        backpropagate tl (List.tl reward)
      end

(** [mcts r] updates tree of root [t] with monte carlo *)
let mcts root =
  for i = 1 to 4 do
    let path = treepolicy root in
    let wins = defaultpolicy (List.hd path) in
    let bppg_aux win = backpropagate path win in
    List.iter bppg_aux wins
  done ;
