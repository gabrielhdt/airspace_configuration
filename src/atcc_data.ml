(***********************************************************************)
(*                                                                     *)
(*                           Atcc_data                                 *)
(*                                                                     *)
(*         David Gianazza, Ecole Nationale de l'Aviation Civile        *)
(*                                                                     *)
(*  Copyright 2017 Ecole Nationale de l'Aviation Civile.               *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Library General Public License.                            *)
(*                                                                     *)
(***********************************************************************)


(*module Smap= Map.Make (struct type t=string let compare=compare end);;*)

type airspace_sector= string
type atc_sector= string

type centre = {
    c_name : string;
    c_sectors : airspace_sector list;
    c_groups : (atc_sector * airspace_sector list) list
  }

type centre_map = centre Util.Smap.t

(* Parse the file data *)

let sep = Str.regexp "[ \t]+"

let get_sdata = fun info ->
  let fields = Str.split sep info in
  fields

let get_cdata = fun info ->
  let fields = Str.split sep info in
  match fields with
  | _long_name :: name :: _n1 :: _n2 :: [] -> name
  | _ -> failwith "Atcc_data.get_cdata"

let get_rgroup = fun info ->
  let fields = Str.split sep info in
  match fields with
  | name :: _nb :: snames -> name, snames
  | _ -> failwith "Atcc_data.get_rgroup"


let centres2smap = fun centres ->
  let addcentre = fun cname snames rgroups result ->
    match rgroups with
      [] -> result
    | _->
      let centre = { c_name = cname; c_sectors = snames; c_groups = rgroups} in
      Util.Smap.add cname centre result
  in
  let rec parse_rgroup = fun l cdata sdata rgroups result ->
    match l with
    | [] -> addcentre cdata sdata rgroups result
    | ("R", info) :: rest ->
	let grname, gr = get_rgroup info in
      	parse_rgroup rest cdata sdata ((grname,gr)::rgroups) result
    | ("S", info) :: rest ->
      	let new_sdata = get_sdata info in
      	parse_rgroup rest cdata new_sdata [] result
    | ("C", info) :: rest ->
	let newresult = addcentre cdata sdata rgroups result
      	and new_cdata = get_cdata info in
      	parse_rgroup rest new_cdata [] [] newresult
    | _ :: rest -> parse_rgroup rest cdata sdata rgroups result
  in
  parse_rgroup centres "" [] [] Util.Smap.empty


(* Read "centre" file *)
let read_line = fun i line ->
  let n = String.length line in
  let code = if n >= i then String.sub line 0 i else ""
  and info = if n >= i then String.sub line i (n - i) else "" in
  code, info

let read_centre = read_line 1

let read_centre_lines = fun file ->
  let chin = open_in file and centres = ref [] in
  try
    while true do
      centres := read_centre (input_line chin) :: !centres
    done;
    failwith "unreachable"
  with End_of_file -> List.rev !centres

let read_centres = fun file -> centres2smap (read_centre_lines file)


(*-------------------------------------------*)
(* Test *)
(* Uncomment test call and compile with:
   ocamlc -o test_atcc str.cma atcc_data.ml
*)

let test () =
  let cmap= read_centres "/projet/donnees/ca/2016/2016-12-08/ca/centres" in
  Util.Smap.iter
    (fun cname c ->
      Printf.printf "%s :\n" cname;
      List.iter
        (fun (atc_sector, ls) ->
          Printf.printf "\t%s :" atc_sector;
          List.iter (Printf.printf " %s") ls;
          Printf.printf "\n")
        c.c_groups;
      Printf.printf "\n")
    cmap;
  flush stdout;;

(* test () *)
