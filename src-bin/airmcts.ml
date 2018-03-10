let () =
  Arg.parse Options.speclist Options.anon_fun (Options.usage Sys.argv.(0)) ;
  let sc = Scenario.load !Options.scpath in
  let module Env = struct
    let tmax = !Options.tmax
    let workload = Scenario.workload sc
  end
  in
  let module Support = Airconf.Make(Env) in
  let s15 = Util.Sset.add "1" (Util.Sset.add "5" Util.Sset.empty) in
  let s32 = Util.Sset.add "3" (Util.Sset.add "2" Util.Sset.empty) in
  let s4 = Util.Sset.add "4" Util.Sset.empty in
  let initial_partition = [(s15, ["d"]); (s32, ["a"]); (s4, ["s4"])] in
  let module Airmcts = Mcts.Make(Support) in
  let root = Airmcts.make_node @@ Support.make_root initial_partition in

  let b_path = (Airmcts.best_path_robust root !Options.nsim) in
  Printf.printf "%d\n" (List.length b_path);

  Partitions.print_partitions
    (List.map (fun s -> Support.get_partitions s)
       (List.map (fun tree -> Airmcts.get_state tree) b_path
       )
    )
