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

type 'a data = {
  cost: float;
  parent: 'a option;
  depth: int;
  expanded: bool }  

type 'a t = ('a, 'a data) Hashtbl.t

let init u0 c0=
  let (tbl:('a, 'a data) Hashtbl.t)= Hashtbl.create 7 in
  Hashtbl.replace tbl u0
    {cost=c0;parent= None;depth=0;expanded= false};
  tbl

let store_state memory s cost parent =
  let parent_depth=
    try (Hashtbl.find memory parent).depth
    with Not_found -> failwith "store_state: parent data not found" in
  let expanded= false in
  Hashtbl.replace memory s {cost=cost;parent= Some parent;
			    depth=parent_depth+1;expanded= expanded}

let get_cost memory s =  (Hashtbl.find memory s).cost

let get_depth memory s = (Hashtbl.find memory s).depth

let already_expanded memory s = (Hashtbl.find memory s).expanded

let tag_as_expanded memory s =
  try
    let data= Hashtbl.find memory s in
    Hashtbl.replace memory s {data with expanded=true}
  with Not_found -> failwith "put_in_d"

let get_path memory s =
  let rec loop s =
    let data= Hashtbl.find memory s in
    match data.parent with
      None -> [s]
    | Some u -> s:: loop u in
  List.rev (loop s)

let mem = Hashtbl.mem

let elements memory =
  Hashtbl.fold (fun key _data acc -> key::acc) memory []

let closed_list memory =
  Hashtbl.fold
    (fun key data acc -> if data.expanded then key::acc else acc)
    memory []
