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
  val get_time : t -> int
end

module Make (Supp : Support) = struct

  type state = Supp.t

  (* Single player constant, ensures that rarely explored nodes are still
   * considered promising *)
  let _spmctsc = 1.2

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
      node.q +. _a /. sqrt (float node.n +. 1.)

    (* some functions that define how to select the final path
       (best q, best n of best lcb)*)
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

    let newm2 m2 newval oldmean newmean =
      let delta = newval -. oldmean
      and delta2 = newval -. newmean in
      m2 +. delta *. delta2

    let ucb father child =
      let log_over_armcount = (log (float (father.n + 1))) /.
                              float (child.n + 1)
      and sdsq = child.m2 /. float child.n in
      let tuning = min 0.25 (sdsq +. sqrt (2. *. log_over_armcount))
      in child.q +. sqrt (_spmctsc *. log_over_armcount *. tuning)

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

  (* TODO The two functions below could be grouped in one which would expand the
   * node or raise an exception *)
  let expandable node =
    (* Parses the children to see if one has not been visited *)
    let rec loop = function
      | [] -> All_visited
      | hd :: tl -> if hd.n = 0 then True else loop tl
    in
    if Supp.terminal node.state then Term else loop node.children

  (** [expand t] returns node which must be visited among children of [t] *)
  let expand node =
    let rec loop = function
      | [] -> failwith "no nodes to expand"
      | hd :: tl -> if hd.n = 0 then hd else loop tl
    in
    loop node.children

  (** [select t a] builds a path toward most urgent node to expand *)
  let rec select node ancestors =
    force_deploy node ;
    match expandable node with
    | True -> (* Select randomly a node among those not visited *)
        let leftalone = Auxfct.random_elt @@ List.filter (fun c -> c.n = 0)
            node.children in
        leftalone :: node :: ancestors
    | Term -> node :: ancestors
    | All_visited ->
      match node.children with
      | (hd :: tl) -> let favourite = SelPol.best_child node in
        select favourite (node :: ancestors)
      | [] -> ancestors

  let treepolicy root =
    let path = select root [] in
    if expandable (List.hd path) = True then
      let exnode = expand (List.hd path) in exnode :: path
    else path

  (** [default_policy t] parses the tree [t] randomly until a terminal state
      is found, and returns an evaluation of the path *)
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

  (** [simulate n ns] starting from node [n] gives the results of [ns]
      simulations *)
      (*
  let simulate node nsim =
    let rec loop cnt acc =
      if cnt > nsim then acc
      else loop (cnt + 1) (default_policy node :: acc)
    in
    loop 0 []
         *)

  (** [backpropagate a r] updates ancestors [a] with the reward [r] *)
  let rec backpropagate ancestors reward =
    List.iter (fun e ->
        let oldmean = e.q in
        e.n <- e.n + 1 ;
        e.q <- e.q +. (reward -. e.q) /. (float e.n +. 1.) ;
        e.m2 <- SelPol.newm2 e.m2 reward oldmean (e.q /. (float (e.n + 1)))
      ) ancestors

  (** [mcts r] updates tree of root [t] with monte carlo *)
  let mcts root maxtime =
    let flag = ref false in
    while not !flag do
      let path = treepolicy root in
      let reward = simulate (List.hd path) in
      List.iter (fun n -> backpropagate path reward) path ;
      flag := stop maxtime
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
