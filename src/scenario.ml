(* Profile of a sector over time *)
type t = (
  string * (* Name of sector *)
  int list (* Number of aircraft at each time step *)
) list

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
