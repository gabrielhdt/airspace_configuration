type 'a tree =
  | Leaf
  | Node of 'a * (('a tree) list)

let build_tree root produce term =
  let rec inner node =
    if term node
    then Leaf
    else Node (node, List.map (fun e -> inner e) (produce node))
  in
  inner root

let len tree =
  let rec inner tree accu =
    match tree with
    | Leaf -> accu
    | Node (_, l) -> inner (List.hd l) (accu + 1)
  in
  inner tree 0

let () =
  Arg.parse Options.speclist Options.anon_fun (Options.usage Sys.argv.(0)) ;
  Printf.printf "%s%!\n" !Options.scpath;
  let sc = Scenario.load !Options.scpath in
  let s15 = Util.Sset.add "1" (Util.Sset.add "5" Util.Sset.empty) in
  let s32 = Util.Sset.add "3" (Util.Sset.add "2" Util.Sset.empty) in
  let s4 = Util.Sset.add "4" Util.Sset.empty in
  let initial_partition = [
    (s15, [("d" : Util.Smap.key) ]) ;
    (s32, [("a" : Util.Smap.key) ]) ;
    (s4, [("s4" : Util.Smap.key) ])] in

  let module Env = struct
    let tmax = !Options.maxsearch
    let alpha = !Options.alpha
    let beta = !Options.beta
    let gamma = !Options.gamma
    let lambda = !Options.lambda
    let theta = !Options.theta
    let init = initial_partition
    let workload = Scenario.workload sc
  end in

  let module MctsParam = struct
    let expvexp = !Options.expvexp
  end in

  let module Support = Airconf.Make(Env) in

  let root = Support.init in
  let my_tree = build_tree root Support.produce Support.terminal in
  Printf.printf "%d\n" (len my_tree)
