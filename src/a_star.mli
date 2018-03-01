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

(** An implementation of the A* algorithm, with a functorial interface.*)

type 'a state = 'a

type prio= float
      
(* The type for a state (i.e. a node of the searched tree) *)

(***
type 'a user_fun = {
  do_at_extraction:
      'a Pqueue.t -> 'a Memory.t -> 'a state -> unit;
(** [do_at_extraction q memory u] should return [unit]. The arguments of this function should be : [q] the priority queue for the states that remain to be explored,  [memory] the memory table storing all relevant state data, and [u] the current node.*)
  do_at_insertion: 'a state -> 'a state -> unit;
(** [do_at_insertion u v] should also return [unit]. Its arguments are [u] the current node, and [v] a 'son' about to be inserted in the priority queue. *)
}
(** The type for a user-chosen argument allowing to perform some actions, like printing or displaying information, at specific steps of the algorithm (extraction, or insertion of a node from/to the priority queue. [user_fun] contains two functions that must be provided by the user for that purpose.*)
      
val search : 'a user_fun -> 'a state -> ('a state -> bool) -> ('a state -> 'a state list) -> ('a state -> 'a state -> float) -> ('a state -> float) -> 'a state list
(** [search user_fun u0 is_goal next k h] searches the state space using the A* algorithm, starting at initial state [u0]. The search terminates at state [u] when [is_goal u] is true. The function returns the path (a list of states) between [u0] and [u] as a result. [next] is the production rule allowing to produce the neighbors of the current state (i.e the 'sons' of the current 'father' node). [k] is the function returning the cost of a transition from one state to another. [h] is the heuristic function returning the estimated cost to reach the goal. [user_fun] contains some functions that are called at specific steps of the algorithms, when extracting a node from the priority queue, or when inserting one. These functions can be defined by the user, so as to print or display all information that he/she wishes.*)
***)

(***)
type 'a user_fun = {
    do_at_extraction:
      (prio, 'a) Pqueue.t -> 'a Memory.t -> 'a state -> unit;
(** [do_at_extraction q memory u] should return [unit]. The arguments of this function should be : [q] the priority queue for the states that remain to be explored,  [memory] the memory table storing all relevant state data, and [u] the current node.*)
  do_at_insertion: 'a state -> 'a state -> unit;
(** [do_at_insertion u v] should also return [unit]. Its arguments are [u] the current node, and [v] a 'son' about to be inserted in the priority queue. *)
}
(** The type for a user-chosen argument allowing to perform some actions, like printing or displaying information, at specific steps of the algorithm (extraction, or insertion of a node from/to the priority queue. [user_fun] contains two functions that must be provided by the user for that purpose.*)
      
val search : 'a user_fun -> 'a state -> ('a state -> bool) -> ('a state -> 'a state list) -> ('a state -> 'a state -> float) -> ('a state -> float) -> 'a state list
(** [search user_fun u0 is_goal next k h] searches the state space using the A* algorithm, starting at initial state [u0]. The search terminates at state [u] when [is_goal u] is true. The function returns the path (a list of states) between [u0] and [u] as a result. [next] is the production rule allowing to produce the neighbors of the current state (i.e the 'sons' of the current 'father' node). [k] is the function returning the cost of a transition from one state to another. [h] is the heuristic function returning the estimated cost to reach the goal. [user_fun] contains some functions that are called at specific steps of the algorithms, when extracting a node from the priority queue, or when inserting one. These functions can be defined by the user, so as to print or display all information that he/she wishes.*)
(***)
    
    
(*** Essai **
type 'a user_fun2 = 
  (float, (float * 'a list)) Pqueue.t -> 
  ('a, bool) Hashtbl.t ->
  'a list ->  
  unit

val search2 : 'a user_fun2 -> 'a state -> ('a state -> bool) -> ('a state -> 'a state list) -> ('a state -> 'a state -> float) -> ('a state -> float) -> 'a state list
***)

    
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
(***  Uncomment when using Pqueue implementated with Map
  val do_at_extraction:
      user_param -> state Pqueue.t -> state Memory.t -> state -> unit
***)      
(***)
  val do_at_extraction:
      user_param -> (prio,state) Pqueue.t -> state Memory.t -> state -> unit
(***)
end

module type Astar = sig
  include Model
  val search: user_param -> state list
end

module Make: functor (M:Model) -> (Astar with type state=M.state and type user_param= M.user_param)    
