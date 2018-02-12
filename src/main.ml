let usage = "Usage: " ^ Sys.argv.(0) ^ " " ^
            "[-scenario string]"

let scpath = ref ""

let speclist = [
  ("-scenario", Arg.Set_string scpath,
   "scenario to load and play") ;
]
let () =

  let s01 = Util.Sset.add "3" (Util.Sset.add "2"
      (Util.Sset.add "1" Util.Sset.empty)) in
  let s02 = Util.Sset.add "5"
      (Util.Sset.add "4" Util.Sset.empty) in
  let initial_partition = [(s01, ["e"]); (s02, ["c"])] in
  let root = Mcts.make_node (Airconf.make_root initial_partition) in

  let b_path = (Mcts.best_path_secure root 1000) in
  Printf.printf "%d\n" (List.length b_path);

  Partitions.print_partitions
    (List.map (fun s -> Airconf.get_partitions s)
       (List.map (fun tree -> Mcts.get_state tree) b_path
       )
    )
