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

(** Memory table to store and retrieve state data. *)

type 'a t
(** An abstract type for a memory table (i.e. you don't have to know how it is implemented)*)

val init : 'a -> float -> 'a t
(** [init u0 c0] initializes a memory table with initial state [u0] and its associated cost [c0]. *)

val store_state : 'a t -> 'a -> float -> 'a -> unit
(** [store_state memory u cost father] stores the current state [u] and its cost into [memory], as well as its parent state [father]. *)

val already_expanded : 'a t -> 'a -> bool
(** [already_expanded memory u] returns [true] is state [u] has already been expanded. *)

val tag_as_expanded: 'a t -> 'a -> unit
(** [tag_as_expanded memory u] memorizes the fact that [u] has been expanded. *)

val get_cost : 'a t -> 'a -> float
(** [get_cost memory u] returns the cost associated to [u] in [memory]. Raises exception [Not_found] if no data is associated to [u] in [memory]. *)

val get_depth : 'a t -> 'a -> int
(** [get_depth memory u] returns the depth of [u] in the tree (i.e. the length or the path from the initial state to the current state).*)

val get_path : 'a t -> 'a -> 'a list
(** [get_path memory u] returns the list of successive states between the initial state and the current state [u], considering the history of state transitions stored in [memory]. *)

val mem : 'a t -> 'a -> bool
(** [mem memory u] returns true if element [u] is in [memory] *)

val elements : 'a t -> 'a list
(** [elements memory] returns the list of states in [memory] *)

val closed_list : 'a t -> 'a list
(** [closed_list memory] returns the list of expanded states in [memory] *)
