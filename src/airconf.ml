(* max nb of aircrft in a controlled sector *)
let _threshold = 10.

type t = {
  time : int ; (* Used to determine whether the node is terminal *)
  partition : (Util.Sset.t * Util.Smap.key list) list
}

let conf_cost conf s =
  let time = conf.time in
  List.fold_left (fun accu elt ->
      let subset = Util.Sset.elements (fst elt) in
      let current_cost = List.fold_left (fun ac m ->
          ac +. float (Scenario.f s time m)
        ) 0. subset in
      accu +. current_cost
    ) 0. conf.partition

let trans_cost child father =
  if child.partition = father.partition then 0. else 1.

let produce t = []

let terminal t = true

let path_cost scenario path c_cost t_cost =
  let rec partial_cost current_path accu =
    match current_path with
    | [] -> accu
    | [hd] -> accu +. c_cost hd scenario
    | hd::tl -> partial_cost tl
                  ( accu +.
                    (c_cost hd scenario) +.
                    (t_cost hd ( List.hd tl )))
  in
  -. (partial_cost path 0.)
