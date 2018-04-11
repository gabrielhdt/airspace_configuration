let () =
  Arg.parse Options.speclist Options.anon_fun (Options.usage Sys.argv.(0)) ;
  let sc = Scenario.load !Options.scpath in
  let s =  (Util.Sset.add "AP"
              (Util.Sset.add "OG"
                 (Util.Sset.add "OT"
                    (Util.Sset.add "OY"
                       (Util.Sset.add "RT"
                          (Util.Sset.add "TB"
                            (Util.Sset.add "TE"
                              (Util.Sset.add "TH"
                                (Util.Sset.add "TN"
                                   (Util.Sset.add "TP"
                                      (Util.Sset.add "UK"
                                         (Util.Sset.add "UZ" Util.Sset.empty)))))))))))) in
  (* let s15 = Util.Sset.add "1" (Util.Sset.add "5" Util.Sset.empty)
  and s32 = Util.Sset.add "3" (Util.Sset.add "2" Util.Sset.empty)
  and s4 = Util.Sset.add "4" Util.Sset.empty in *)
  let initial_partition = [
    (s, [("RPW") ])
  ] in

  let module Env = struct
    let horizon = !Options.horizon
    let alpha = !Options.alpha
    let beta = !Options.beta
    let gamma = !Options.gamma
    let lambda = !Options.lambda
    let theta = !Options.theta
    let init = initial_partition
    let ctx = Scenario.ctx
    let sectors = Scenario.sectors
    let workload = Scenario.workload sc
  end in

  let module MctsParam = struct
    let lapse = !Options.timeperstep
    let expvexp = !Options.expvexp
  end in

  let module Support = Airconf.Make(Env) in
  let module Airmcts = Mcts.Make(Support)(MctsParam) in
  let path = Airmcts.buildpath Airmcts.root !Options.nsteps
      Airmcts.select_robust in
  let pathcost = List.fold_left (fun acc elt ->
      acc +. (Support.cost @@ Airmcts.get_state elt)) 0. path in
  print_string "\n=========\n" ;
  Printf.printf "final cost: %f (for a length of %d)\n" pathcost (List.length path);
  if !Options.verbose then
    Partitions.print_partitions (List.map (fun s ->
        Support.get_partitions s) (List.map (fun tree ->
        Airmcts.get_state tree)
      path)
    )
