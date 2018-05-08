(***********************************************************************)
(*                                                                     *)
(*                     Astar (A* algorithm)                            *)
(*                                                                     *)
(*         David Gianazza, Ecole Nationale de l'Aviation Civile        *)
(*                                                                     *)
(*  Copyright 2017 Ecole Nationale de l'Aviation Civile.               *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Library General Public License.                            *)
(*                                                                     *)
(***********************************************************************)

type 'a state = 'a

type prio= float

type 'a user_fun = {
  do_at_extraction:
    (prio, 'a) Pqueue.t -> 'a Memory.t -> 'a state -> unit;
  do_at_insertion: 'a state -> 'a state -> unit;
}

(*-------------------*)
(* The A* algorithm  *)

exception Eureka

(*
let search user_fun u0 is_goal next k h =
  (* Version avec une boucle récursive *)
  let cost0= 0. in
  let f0= cost0 +. h u0 in
  let m= Memory.init u0 cost0 in
  let rec loop q =
    try
      let (_prio,u,q)= Pqueue.extract q in
      user_fun.do_at_extraction q m u;
      if is_goal u then (* Reconstruire le chemin complet *)
        Memory.get_path m u
      else (* Calculer les noeuds fils, evaluer, et inserer dans la file *)
	if not (Memory.already_expanded m u) then (
	  Memory.tag_as_expanded m u;
	  let offspring = next u in
	  let cu = Memory.get_cost m u in
	  let new_q =
	    List.fold_left
	      (fun q v ->
		let c= cu +. k u v in
		if not (Memory.mem m v) || Memory.get_cost m v > c then
		  begin
		    let e= c +. h v in
		    Memory.store_state m v c u;
		    user_fun.do_at_insertion u v;
		    Pqueue.insert e v q
		  end
		else q)
	      q offspring in
	  loop new_q )
	else loop q
    with Pqueue.Empty -> failwith "Pas de solution" in
  let q = Pqueue.insert f0 u0 Pqueue.empty in
  loop q
*)

let search user_fun u0 is_goal next k h =
  let count = ref 0 in
  let cost0= 0. in
  let f0= cost0 +. h u0 in
  let m= Memory.init u0 cost0 in

  let q = ref (Pqueue.insert f0 u0 Pqueue.empty) in
  let path = ref [] in
  try
    while not (Pqueue.is_empty !q) do
      let (c,u,new_q) = Pqueue.extract !q in
      q := new_q;
      if is_goal u then (path := Memory.get_path m u;
                         raise Eureka)
      else
      if (not (Memory.already_expanded m u)) then
        (Memory.tag_as_expanded m u;
         let ls = next u in
         List.iter (fun v ->
      incr count;
             let cv = k u v +. h v in
             (* let cv = Cost_functions.cost_full_partition
                v.State.partition v.t u_p.data u_p.nnet in *)
             (* let cv = Utils.add_tuple (Memory.get_cost m u) cv in *)
             if ((not (Memory.mem m v))|| (Memory.get_cost m v > cv)) then
               (Memory.store_state m v cv u;
                q := Pqueue.insert ( cv +. (h v)) v !q;)
           ) ls)
    done;
    Printf.printf "Empty priority queue";
    []
  with Eureka -> !path;;



(*----------------------*)
(* Functorial interface *)

module type Model= sig
  type state
  type user_param

  val initial_state: user_param -> state

  val is_goal: user_param -> state -> bool
  val next: user_param -> state -> state list
  val k: user_param -> state -> state -> float
  val h: user_param -> state -> float

  val do_at_insertion:
    user_param -> state -> state -> unit

  val do_at_extraction:
    user_param -> (prio, state) Pqueue.t -> state Memory.t -> state -> unit
end

module type Astar = sig
  include Model
  val search: user_param -> state list
end


module Make (M:Model):(Astar with type state=M.state
                              and type user_param= M.user_param) = struct
  include M

  let search user_param =
    let u0= M.initial_state user_param in
    let is_goal = M.is_goal user_param
    and next= M.next user_param
    and k= M.k user_param
    and h= M.h user_param in
    let user_fun= {do_at_extraction= M.do_at_extraction user_param;
                   do_at_insertion= M.do_at_insertion user_param } in
    search user_fun u0 is_goal next k h
end
