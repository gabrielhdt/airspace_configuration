
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
  let module Support = Airconf.Make(Env) in

  let greedy_traversal root =
    let rec loop nroot acc =
      if Support.terminal nroot then acc else
        let children = Support.produce nroot in
        let favourite = Auxfct.argmax (fun n m ->
            if Support.cost n <= Support.cost m then n else m) children in
        loop favourite (favourite :: acc) in
    loop root [root] in

  let path  = greedy_traversal Support.init in
  let totcost = List.fold_left (fun acc n -> acc +. Support.cost n) 0. path in
  Printf.printf "%f\n" totcost;

  if !Options.verbose then
      List.iter (fun x ->
        Partitions.print_partition @@ Support.get_partitions x) (List.rev path)
