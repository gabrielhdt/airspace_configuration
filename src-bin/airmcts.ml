let () =
  Arg.parse Options.speclist Options.anon_fun (Options.usage Sys.argv.(0)) ;
  let sc = Scenario.load !Options.scpath in
  let s15 = Util.Sset.add "1" (Util.Sset.add "5" Util.Sset.empty) in
  let s32 = Util.Sset.add "3" (Util.Sset.add "2" Util.Sset.empty) in
  let s4 = Util.Sset.add "4" Util.Sset.empty in
  let initial_partition = [
    (s15, [("d" : Util.Smap.key) ]) ;
    (s32, [("a" : Util.Smap.key) ]) ;
    (s4, [("s4" : Util.Smap.key) ])] in
  let module Env = struct
    let tmax = !Options.tmax
    let alpha = !Options.alpha
    let beta = !Options.beta
    let gamma = !Options.gamma
    let lambda = !Options.lambda
    let theta = !Options.theta
    let init = initial_partition
    let workload = Scenario.workload sc
  end
  in
  let module Support = Airconf.Make(Env) in
  let module Airmcts = Mcts.Make(Support) in
  let b_path = (Airmcts.best_path_secure Airmcts.root !Options.nsim) in

  Partitions.print_partitions
    (List.map (fun s -> Support.get_partitions s)
       (List.map (fun tree -> Airmcts.get_state tree) b_path)
    )
