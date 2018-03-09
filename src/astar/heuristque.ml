open Scenario

type estim_load =
  | Low
  | Normal
  | High


let e_wl a b c =
  if a > b && a > c then High else if b > a && b > c then Normal else Low

let sc = load "data/scen3.json"

let partition_cost time part =
  let (high, normal, low) = List.fold_left (fun accu sec ->
      let (a, b, c) = accu in
      let (ph, pn, pl) = workload sc time (Util.Sset.elements (fst sec)) in
      let status = e_wl ph pn pl in
      let card = Util.Sset.cardinal (fst sec) in
      match status with
      | High -> (a +. ph *. (float card) ** 2., b, c)
      | Normal -> (a, b +. pn *. (float card) ** (-2.), c)
      | Low -> (a, b, c +. pl *. (float card) ** (-2.))
    ) (0., 0., 0.) part in
  1000. *. high +. 100. *. normal +. 10. *. low
  +. 1. *. (float (List.length part))


let best ctx f le =
  let sons = Partitions.partitions ctx f le in
  List.fold_left (fun accu elt ->
      if partition_cost 5 elt < partition_cost 5 accu then elt else accu
    ) (List.hd sons) sons
