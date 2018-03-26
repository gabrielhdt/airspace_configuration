let () =
  Arg.parse Options.speclist Options.anon_fun (Options.usage Sys.argv.(0)) ;
  let sc = Scenario.load !Options.scpath in
  let s15 = Util.Sset.add "1" (Util.Sset.add "5" Util.Sset.empty) in
  let s32 = Util.Sset.add "3" (Util.Sset.add "2" Util.Sset.empty) in
  let s4 = Util.Sset.add "4" Util.Sset.empty in
  let initial_partition = [
    (s15, [("d") ]) ;   (s32, [("a") ]) ;   (s4, [("s4") ])
  ] in

  let module Env = struct
    let tmax = !Options.horizon
    let alpha = !Options.alpha
    let beta = !Options.beta
    let gamma = !Options.gamma
    let lambda = !Options.lambda
    let theta = !Options.theta
    let init = initial_partition
    let workload = Scenario.workload sc
  end in

  let module MctsParam = struct
    let lapse = !Options.timeperstep
    let expvexp = !Options.expvexp
  end in

  let module Support = Airconf.Make(Env) in
  let module Airmcts = Mcts.Make(Support)(MctsParam) in
  let rec buildpath cnt acc =
    if cnt >= !Options.nsteps then acc else
      begin
        let newtree = List.hd acc in
        Airmcts.mcts newtree ;
        Airmcts.print_children newtree ;
        buildpath (cnt + 1) (Airmcts.select_robust newtree :: acc)
      end
  in
  let path = buildpath 0 [Airmcts.root] in
  let pathcost = List.fold_left (fun acc elt ->
      acc +. (Support.cost @@ Airmcts.get_state elt)) 0. path in
  print_string "\n=========\n" ;
  Printf.printf "final cost: %f (for a length of %d)\n" pathcost !Options.nsteps
