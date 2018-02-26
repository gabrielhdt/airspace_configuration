module type S = sig
  type state
  type tree
  val best_path_max : tree -> int -> tree list
  val best_path_secure : tree -> int -> tree list
  val best_path_robust : tree -> int -> tree list
  val make_node : state -> tree

  (********************************* DEBUG ***********************************)
  val get_state : tree -> state
end

module type SuppS = sig
  type t
  val produce : t -> t list
  val reward : t -> float
  val terminal : t -> bool
  val print : t -> unit
  val make_root : (Util.Sset.t * Util.Smap.key list) list -> t
end

module Make (Support : SuppS) = struct

  type state = Support.t
  (* UCT mcts algo *)
  let _beta = 1.

  type tree = {
    state : state ;
    mutable q : float ;
    mutable n : int ;
    mutable children : tree list
  }

  (* state of the node regarding expansion *)
  type expandability =
    | True
    | Term
    | All_visited

  let make_node state =
    {
      state = state ;
      q = 0. ;
      n = 0 ;
      children = []
    }

  module type WinPolicy =
    sig
      val lcb : tree -> float
      val max : tree list -> tree
      val robust : tree list -> tree
      val secure : tree list -> tree
    end

  (* Contains ways to select final best path *)
  module WinPol : WinPolicy = struct

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
    (* TODO: add single player mcts 3rd term *)
    let ucb beta father child =
      child.q /. float child.n +.
      beta *. sqrt (2. *. log (float father.n) /. ((float child.n) +. 1.))

    let best_child father =
      Auxfct.argmax (fun ch1 ch2 ->
          if ucb _beta father ch1 >= ucb _beta father ch2 then ch1 else ch2
        ) father.children
  end

  (* [stop p] returns whether the mcts should stop on this path. Returns true
   * iff node is terminal for the support and all its brothers are terminal and
   * visited *)
  let stop path =
    let last = List.hd path in if not (Support.terminal last.state) then false
    else let father = List.hd (List.tl path) in
      let all_visited = List.fold_left (fun acc elt -> acc && elt.n > 0) true
          father.children
      in
      all_visited


  (* [produce t] creates the list of reachable nodes from [t] *)
  let produce node =
    let next_states = Support.produce node.state in
    List.map (fun s -> { state = s ; q = 0. ; n = 0 ; children = [] })
      next_states

  (** [force_deploy t] tries to add children from a production rule and saves
      them into the node *)
  let force_deploy node =
    match node.children with
    | [] -> node.children <- produce node
    | _ :: _ -> ()

  (* TODO The two functions below could be grouped in one which would expand the
   * node or raise an exception *)
  let expandable node =
    (* Parses the children to see if one has not been visited *)
    let rec loop = function
      | [] -> All_visited
      | hd :: tl -> if hd.n = 0 then True else loop tl
    in
    if Support.terminal node.state then Term else loop node.children

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
    match expandable node with
    | True | Term -> node :: ancestors
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

  (** [simulate_once t] parses the tree [t] randomly until a terminal state
      is found, and returns an evaluation of the path *)
  let simulate_once node =
    let rec loop next_node accu =
      let reward = Support.reward next_node.state in
      if Support.terminal next_node.state then reward +. accu
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
  let simulate node nsim =
    let rec loop cnt acc =
      if cnt > nsim then acc
      else loop (cnt + 1) (simulate_once node :: acc)
    in
    loop 0 []

  (** [backpropagate a w] updates ancestors [a] with the result win [w] *)
  let rec backpropagate ancestors reward n =
    List.iter ( fun e -> e.q <- e.q +. reward; e.n <- e.n + n ) ancestors

  (** [mcts r] updates tree of root [t] with monte carlo *)
  let mcts root nsim =
    let flag = ref false in
    while not !flag do
      let path = treepolicy root in
      let wins = simulate (List.hd path) nsim in
      let reward = List.fold_left (fun accu e -> accu +. e) 0. wins in
      let n = List.length wins in (* should be nsim *)
      backpropagate path reward n ;
      flag := stop path
      (* flag := Airconf.terminal (List.hd path).state *)
    done

  let best_path root criterion =
    let rec aux current_node accu =
      match current_node.children with
      | [] -> accu
      | children ->
        Printf.printf "------New selection--------\n" ;
        List.iter print_node children ;
        List.iter (fun n -> Support.print n.state) children ;
        let best_ch = criterion children in
        aux best_ch (best_ch :: accu)
    in
    aux root [root]

  let best_path_max root nsim =
    mcts root nsim;
    best_path root WinPol.max

  let best_path_secure root nsim =
    mcts root nsim;
    best_path root WinPol.secure

  let best_path_robust root nsim =
    mcts root nsim;
    best_path root WinPol.robust
end
