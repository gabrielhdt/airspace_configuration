(* creation of the context needed for  our scenario *)
let l =
    [ ("s1",["1"]);
      ("s2",["2"]);
      ("s3",["3"]);
      ("s4",["4"]);
      ("s5",["5"]);
      ("a",["2";"3"]);
      ("b",["3";"4"]);
      ("c",["4";"5"]);
      ("d",["1";"5"]);
      ("e",["1";"2";"3"]);
      ("f",["1";"2";"3";"4";"5"]);
      ("g",["1";"2"])  ]

let sectors = ["1"; "2"; "3"; "4"; "5"]

let ctx = Partitions.make_context l

(* Profile of a sector over time *)
type t = (
  string * (* Name of sector *)
  int list (* Number of aircraft at each time step *)
) list

(* Should be in scenario file *)
let _hthr = 12.
let _lthr = 8.

(* [secjs2telt s] converts a tuple (sector name, aircraft in sector through
 * time in a yojson format) to a tuple (sector name, number of aircraft in
 * sector through time *)
let secjs2telt = function
  | (n, t) -> let timelstjs = Yojson.Basic.Util.to_list t in
    let tlst = List.map (fun jsint -> Yojson.Basic.Util.to_int jsint) timelstjs
    in
    n, tlst

let load path =
  if not (Sys.file_exists path) then failwith "not valid scenario file" ;
  let scjson = Yojson.Basic.from_file path in
  let seclist = Yojson.Basic.Util.to_assoc scjson in
  (List.map secjs2telt seclist : t)

let f t time sec =
  let data = List.assoc sec t in
  List.nth data time

let workload sc time secs =
  let amount =
    List.fold_left (fun acc elt -> acc +. (float @@ f sc time elt)) 0. secs in
  if amount > _hthr then 1., 0., 0.
  else if amount > _lthr then 0., 1., 0.
  else 0., 0., 1.
