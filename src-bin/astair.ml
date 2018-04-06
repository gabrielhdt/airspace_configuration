let _count = ref 0

let user () = incr _count

let () =
  Arg.parse Options.speclist Options.anon_fun (Options.usage Sys.argv.(0)) ;
  let sc = Scenario.load !Options.scpath in
  (* Inital state *)
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
    let tmax = !Options.horizon
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
  let module AirSupp = Airconf.Make(Env) in

  let module Heuri = Heuristic.Make(Env)(AirSupp) in

  let module AstAir = struct
    type state = AirSupp.t
    type user_param = unit

    let initial_state _ = AirSupp.init
    let is_goal _ st = AirSupp.terminal st
    let next _ = AirSupp.produce
    let k _ st1 st2 = AirSupp.cost st2
    let h _ st = Heuri.h (AirSupp.get_time st)
    (* let h _ _ = 0. *)
    let do_at_insertion _ _ _ = user ()
    let do_at_extraction _ _ _ _ = ()
  end in

  let module AstarForAir = A_star.Make(AstAir) in
  let b_path = AstarForAir.search () in

  let total_reward = List.fold_left (fun accu e ->
      accu +. AirSupp.cost e) 0. b_path in
  Printf.printf "reward best path : %f\n" (total_reward);

  Printf.printf "path length : %d\nnb visited nodes : %d\n" (List.length b_path) !_count;

  if !Options.verbose then
  List.iter (fun x -> Partitions.print_partition @@ AirSupp.get_partitions x)
    b_path
  else ()
