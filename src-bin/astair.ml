let () =
  Arg.parse Options.speclist Options.anon_fun (Options.usage Sys.argv.(0)) ;
  let sc = Scenario.load !Options.scpath in
  (* Inital state *)
  let s15 = Util.Sset.add "1" (Util.Sset.add "5" Util.Sset.empty)
  and s32 = Util.Sset.add "3" (Util.Sset.add "2" Util.Sset.empty)
  and s4 = Util.Sset.add "4" Util.Sset.empty in
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
  end in
  let module AirSupp = Airconf.Make(Env) in

  let module AstAir = struct
    type state = AirSupp.t
    type user_param = unit

    let initial_state _ = AirSupp.init
    let is_goal _ st = AirSupp.terminal st
    let next _ = AirSupp.produce
    let k _ st1 st2 = AirSupp.reward st2 +. 10. *. if st2 = st1 then 1. else 0.
    let h _ st = AirSupp.reward st
    let do_at_insertion _ _ _ = ()
    let do_at_extraction _ _ _ _ = ()
  end in
  let module AstarForAir = A_star.Make(AstAir) in
  let final_state = AstarForAir.search () in
  List.iter (fun x -> Partitions.print_partition @@ AirSupp.get_partitions x)
    final_state
