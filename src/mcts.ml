let _nodecount = ref 0
let _branchfactor = ref 0

module type S = sig
  type state
  type tree
  val root : tree
  val best_path_max : tree -> float -> tree list
  val best_path_secure : tree -> float -> tree list
  val best_path_robust : tree -> float -> tree list

  (********************************* DEBUG ***********************************)
  val get_state : tree -> state
end

module type Support = sig
  type t
  val init : t
  val produce : t -> t list
  val reward : t -> float
  val terminal : t -> bool
  val print : t -> unit
end

module type MctsParameters = sig
  val expvexp : float
end

module Make (Supp : Support) (MctsParam : MctsParameters) = struct

  type state = Supp.t

  (* Single player constant, ensures that rarely explored nodes are still
   * considered promising *)
  let _spmctsc = 2.

  type tree = {
    state : state ;
    mutable q : float ; (* Reward expectancy *)
    mutable n : int ; (* Number of times the node has been selected *)
    mutable m2 : float ; (* Used to compute standard deviation *)
    mutable children : tree list
  }

  (* state of the node regarding expansion *)
  type expandability =
    | True
    | Term
    | All_visited

  let root = { state = Supp.init ; q = 0. ; n = 0 ; m2 = 0. ; children = [] }

  (* Contains ways to select final best path *)
  module WinPol = struct

    (* arbitrary cste given by chaslot *)
    let _a = 4.

    (* low confidence bound *)
    let lcb node =
      assert (node.n > 0) ;
      node.q +. _a /. (sqrt @@ float node.n)

    (* some functions that define how to select the final path
       (best q, best n of best lcb) *)
    let cmp_max n1 n2 =
      if n1.q > n2.q then n1 else n2

    (* Compares visit count. Compares [q] in case of equality *)
    let cmp_robust n1 n2 =
      if n1.n > n2.n then n1 else if n1.n < n2.n then n2 else cmp_max n1 n2

    let cmp_secure n1 n2 =
      if lcb n1 > lcb n2 then n1 else n2

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
    let lcb = WinPol.lcb node in
    Printf.printf "%f/%d/%f" node.q node.n lcb ;
    print_newline ()

  let get_state n = n.state
  (*******************************************************************)

  (* Selection policy *)
  module SelPol = struct

    let ucb father child =
      assert (child.n > 0) ;
      let log_over_armcount = (log (float father.n)) /. float child.n
      and sdsq = child.m2 /. float child.n in
      let tuning = min 0.25 (sdsq +. sqrt (2. *. log_over_armcount))
      in child.q +. sqrt (MctsParam.expvexp *. log_over_armcount *. tuning)

    let best_child father =
      Auxfct.argmax (fun ch1 ch2 ->
          if ucb father ch1 >= ucb father ch2 then ch1 else ch2
        ) father.children
  end

  (* [stop p] returns whether the mcts should stop on this path *)
  let stop maxtime = Sys.time () >= maxtime


  (* [produce t] creates the list of reachable nodes from [t] *)
  let produce node =
    let next_states = Supp.produce node.state in
    List.map (fun s -> { state = s ; q = 0. ; n = 0 ; m2 = 0. ; children = [] })
      next_states

  (** [force_deploy t] tries to add children from a production rule and saves
      them into the node *)
  let force_deploy node =
    match node.children with
    | [] -> node.children <- produce node ;
        incr _nodecount ;
        _branchfactor := !_branchfactor + (List.length node.children)
    | _ :: _ -> ()

  (* [expandable n] returns whether a node [n] can be expanded. A node is
   * expandable if it is non terminal and one of its children has never been
   * selected *)
  let expandable node =
    (* Parses the children to see if one has not been visited *)
    let rec loop = function
      | [] -> All_visited
      | hd :: tl -> if hd.n = 0 then True else loop tl
    in
    if Supp.terminal node.state then Term else loop node.children

  (** [expand n] returns node which must be visited among children of [n] *)
  let expand node =
    let leftalone = List.filter (fun c -> c.n = 0) node.children in
    assert (List.length leftalone > 0) ;
    Auxfct.random_elt leftalone

  (** [select t a] builds a path toward most urgent node to expand *)
  let rec select node ancestors =
    force_deploy node ;
    match expandable node with
    | True | Term -> node :: ancestors
    | All_visited ->
      match node.children with
      | (hd :: tl) -> let favourite = SelPol.best_child node in
        select favourite (node :: ancestors)
      | [] -> ancestors

  let treepolicy root =
    let path = select root [] in
    if Supp.terminal (List.hd path).state then path else
      let exnode = expand (List.hd path) in
      exnode :: path

  (* [simulate n] also called default policy or random walk selects children
   * randomly from node [n] up to a terminal node and returns the associated
   * reward *)
  let simulate node =
    let rec loop next_node accu =
      let reward = Supp.reward next_node.state in
      if Supp.terminal next_node.state then reward +. accu
      else
        begin
          let children = produce next_node in
          let randchild = Auxfct.random_elt children in
          loop randchild (accu +. reward)
        end
    in
    loop node 0.

  (** [backpropagate a r] updates ancestors [a] with the reward [r] *)
  let rec backpropagate ancestors reward =
    List.iter (fun e ->
        e.n <- e.n + 1 ;
        let oldmean = e.q in
        let delta = reward -. oldmean in
        let newmean = oldmean +. delta /. float e.n in
        let delta2 = reward -. newmean in
        e.q <- e.q +. delta /. float e.n ;
        e.m2 <- e.m2 +. delta *. delta2
      ) ancestors

  (** [mcts r] updates tree of root [t] with monte carlo *)
  let mcts root maxtime =
    let flag = ref false in
    while not !flag do
      let path = treepolicy root in
      let reward = simulate (List.hd path) in
      List.iter (fun n -> backpropagate path reward) path ;
      flag := stop maxtime ;
      Cldisp.mctsinfo !_nodecount reward
    done ; Printf.printf "%d nodes deployed\nmean branch factor: %f\n"
      !_nodecount (float !_branchfactor /. float !_nodecount)

  let best_path root criterion =
    let rec aux current_node accu =
      match current_node.children with
      | [] -> accu
      | children ->
        let best_ch = criterion children in
        aux best_ch (best_ch :: accu)
    in
    aux root [root]

  let best_path_max root maxtime =
    mcts root maxtime ;
    best_path root WinPol.max

  let best_path_secure root maxtime =
    mcts root maxtime ;
    best_path root WinPol.secure

  let best_path_robust root maxtime =
    mcts root maxtime;
    best_path root WinPol.robust
end
