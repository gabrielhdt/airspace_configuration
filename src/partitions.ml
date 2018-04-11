(***********************************************************************)
(*                                                                     *)
(*                           Partitions                                *)
(*                                                                     *)
(*         David Gianazza, Ecole Nationale de l'Aviation Civile        *)
(*                                                                     *)
(*  Copyright 2017 Ecole Nationale de l'Aviation Civile.               *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Library General Public License.                            *)
(*                                                                     *)
(***********************************************************************)

open Util

(*-------------------------------------------------*)

type element = string

type group = string

type subset = Sset.t

type compatible_groups = group list

type partition = (subset * compatible_groups) list

type context =
  { gmap: Sset.t Smap.t
  ; (* key= group ID -> data= set of elements *)
    (* [gmap] stores for each group the elements it is made of. *)
  emap: Sset.t Smap.t
  (* key = element -> data= groups containing element *)
  (* [emap] stores for each element the groups it belongs to.*) }

let member gmap e g =
  try Sset.mem e (Smap.find g gmap) with Not_found -> false


let make_emap gmap =
  Smap.fold
    (fun g elts acc ->
      Sset.fold
        (fun e acc ->
          try
            let gset = Smap.find e acc in
            Smap.add e (Sset.add g gset) acc
          with Not_found -> Smap.add e (Sset.singleton g) acc )
        elts acc )
    gmap Smap.empty


let make_context l =
  let gmap =
    List.fold_left
      (fun acc (g, le) -> Smap.add g (set_of_list le) acc)
      Smap.empty l
  in
  {gmap; emap= make_emap gmap}


let update_context ctx elements =
  let new_gmap =
    Smap.fold
      (fun g elts acc ->
        if Sset.exists (fun e -> not (Sset.mem e elements)) elts then acc
        else Smap.add g elts acc )
      ctx.gmap Smap.empty
  in
  {gmap= new_gmap; emap= make_emap new_gmap}


(*--------------------*)
(* Printing functions *)

let list2string l =
  let rec sprint l =
    match l with
    | [s] -> s
    | s :: ls -> Printf.sprintf "%s %a" s (fun _ -> sprint) ls
    | [] -> ""
  in
  sprint l


let set2string x = list2string (Sset.elements x)

let print_map map =
  Smap.iter (fun x s -> Printf.printf "%s: %s\n" x (set2string s)) map


let print_partition l =
  List.iter
    (fun (subset, glist) ->
      Printf.printf "(%s, %s) " (set2string subset) (list2string glist) )
    l ;
  Printf.printf "\n" ;
  flush stdout


let print_partitions ll = List.iter print_partition ll

(*----------------------------------------------------*)
(* Build all possible partitions of a set of elements *)

exception Not_valid

let possible_groups ctx s =
  (* List of groups containing all elements of set [s] *)
  let init = Smap.find (Sset.choose s) ctx.emap in
  Sset.fold (fun e acc -> Sset.inter (Smap.find e ctx.emap) acc) s init


let compatible_groups ctx s others =
  (* List of groups containing all elements of [s] and no element of [others] *)
  let possibles = Sset.elements (possible_groups ctx s) in
  List.filter
    (fun g ->
      let eset = Smap.find g ctx.gmap in
      Sset.is_empty (Sset.inter eset others) )
    possibles


let update_compatible_groups ctx e others =
  (* Remove [e] from the compatible groups of [others].
     The list of compatible groups associated to a subset of elements
     must contain all the elements of this subset, AND NO ELEMENT FROM
     THE OTHER SUBSETS. *)
  List.map
    (fun (eset, glist) ->
      let new_glist = List.filter (fun g -> not (member ctx.gmap e g)) glist in
      match new_glist with [] -> raise Not_valid | _ -> (eset, new_glist) )
    others


let add_element ctx check e partition update acc =
  (* Builds all new partitions obtained by adding a new element [e]
     to one of the subsets of [partition], or by creating a new subset.
     [check] is a function checking the validity of a partition under
     construction (e.g.: partition containing at most N groups).
     The new partitions are accumulated in [acc]. *)
  (*  Printf.printf "Element %s\n" e;flush stdout;*)
  let rec add revleft right acc =
    match right with
    | (elts, glist) :: rest ->
        let new_elts = Sset.add e elts in
        let new_glist = List.filter (member ctx.gmap e) glist in
        let new_acc =
          match new_glist with
          | [] -> acc
          | _ ->
            try
              let new_revleft = update_compatible_groups ctx e revleft in
              let new_rest = update_compatible_groups ctx e rest in
              let new_right = (new_elts, new_glist) :: new_rest in
              let x = List.rev_append new_revleft new_right in
              (*                Printf.printf "Partition length= %d\n" (List.length x); *)
              flush stdout ;
              if check x then update x acc (*x::acc*) else acc
            with Not_valid -> acc
        in
        add ((elts, glist) :: revleft) rest new_acc
    | [] ->
      try
        let elts = Sset.singleton e in
        let all_others =
          List.fold_left
            (fun acc (eset, _) -> Sset.union eset acc)
            Sset.empty (List.rev revleft)
        in
        let glist =
          List.filter
            (fun g ->
              let eset = Smap.find g ctx.gmap in
              Sset.is_empty (Sset.inter eset all_others) )
            (Sset.elements (Smap.find e ctx.emap))
        in
        match glist with
        | [] -> acc
        | _ ->
            let new_revleft = update_compatible_groups ctx e revleft in
            let x = List.rev ((elts, glist) :: new_revleft) in
            (*              Printf.printf "New partition length= %d\n" (List.length x); *)
            flush stdout ;
            if check x then update x acc (*x::acc*) else acc
      with
      | Not_found | Not_valid -> acc
  in
  add [] partition acc


let partitions ctx check_node le =
  (* [le] is the list of elements from which we compute the partitions.
     [check_node] is a function checking if a partition under construction
     is valid or not (e.g.: accept partitions of at most N subsets). *)
  let elements = set_of_list le in
  let ctx = update_context ctx elements in
  let update x acc = x :: acc in
  let rec loop le partition acc =
    match le with
    | [e] -> add_element ctx check_node e partition update acc
    | e :: es ->
        let children = add_element ctx check_node e partition update [] in
        List.fold_left (fun acc child -> loop es child acc) acc children
    | [] -> acc
  in
  loop le [] []


(*---------------------*)
(* Counting partitions *)

let count ctx check_node le =
  (* [le] is the list of elements from which we compute the partitions.
     [check_node] is a function checking if a partition under construction
     is valid or not (e.g.: accept partitions of at most N subsets). *)
  let elements = set_of_list le in
  let ctx = update_context ctx elements in
  let update _x acc = acc + 1 in
  let rec loop le partition acc =
    match le with
    | [e] -> add_element ctx check_node e partition update acc
    | e :: es ->
        let children =
          add_element ctx check_node e partition (fun x acc -> x :: acc) []
        in
        List.fold_left (fun acc child -> loop es child acc) acc children
    | [] -> acc
  in
  loop le [] 0


(*-----------------------------------------------------*)
(* Build sub-partitions according to operational rules *)
(* Permitted transitions:
   Transfer: AB C -> A BC
   Split2:   AB -> A B
   Merge:    A B -> AB *)

let recombine2 ctx nmax s1 s2 =
  let s = Sset.union s1 s2 in
  let ctx = update_context ctx s in
  let rec changed partition =
    match partition with
    | (s, gl) :: rest ->
        not (Sset.equal s s1 || Sset.equal s s2) && changed rest
    | [] -> true
  in
  let check =
    match nmax with
    | Some k -> fun p -> List.length p <= k
    | None -> fun p -> true
  in
  let l = partitions ctx check (Sset.elements s) in
  List.filter changed l


let transfer ctx s1 s2 =
  let l = recombine2 ctx (Some 2) s1 s2 in
  List.filter (fun p -> List.length p = 2) l


let split2 ctx s = recombine2 ctx (Some 2) s Sset.empty

let merge ctx s1 s2 =
  let s = Sset.union s1 s2 in
  let ctx = update_context ctx s in
  try
    let gset = possible_groups ctx s in
    if Sset.is_empty gset then [] else [[(s, Sset.elements gset)]]
  with Not_found -> []


(* No compatible groups *)
(*---------------------------------------------*)
(* Recombine a partition into valid partitions *)
(* different from the initial one              *)
(* type recombination=
    Transfer | Split2 | Merge *)

let recombine ctx partition =
  let add revleft rest sub_partitions acc =
    List.fold_left
      (fun acc sub_partition ->
        let new_partition =
          List.fold_left
            (fun acc x -> x :: acc)
            (List.rev_append revleft rest)
            sub_partition
        in
        new_partition :: acc )
      acc sub_partitions
  in
  let rec outer_loop revleft1 right1 acc =
    match right1 with
    | (s1, gl1) :: rest1 ->
        (* Printf.printf "Sector %s\n" (set2string s1);flush stdout; *)
        let splits = split2 ctx s1 in
        (* Printf.printf "Splits2:\n"; flush stdout; *)
        (* print_partitions splits; *)
        let acc = add revleft1 rest1 splits acc in
        let rec inner_loop revleft2 right2 acc =
          match right2 with
          | (s2, gl2) :: rest2 ->
              let transfers = transfer ctx s1 s2 in
              let merges = merge ctx s1 s2 in
              let new_acc =
                add revleft2 rest2 transfers (add revleft2 rest2 merges acc)
              in
              inner_loop ((s2, gl2) :: revleft2) rest2 new_acc
          | [] -> outer_loop ((s1, gl1) :: revleft1) rest1 acc
        in
        inner_loop revleft1 rest1 acc
    | [] -> acc
  in
  outer_loop [] partition []
