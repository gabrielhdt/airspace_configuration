(* type string = string *)
(* type string = string *)
type centre = {
  c_name : string;
  c_sectors : string list;
  c_groups : (string * string list) list;
}
type centre_map = centre Util.Smap.t
val sep : Str.regexp
val get_sdata : string -> string list
val get_cdata : string -> string
val get_rgroup : string -> string * string list
val centres2smap : (string * string) list -> centre Util.Smap.t
val read_line : int -> string -> string * string
val read_centre : string -> string * string
val read_centre_lines : string -> (string * string) list
val read_centres : string -> centre Util.Smap.t
val test : unit -> unit
