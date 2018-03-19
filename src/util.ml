
module Sset= Set.Make (struct type t=string let compare=compare end);;
module Smap= Map.Make (struct type t=string let compare=compare end);;

let set_of_list l=
  List.fold_left (fun acc x -> Sset.add x acc) Sset.empty l;;
