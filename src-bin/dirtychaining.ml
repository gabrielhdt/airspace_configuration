let () =
  Arg.parse Options.speclist Options.anon_fun (Options.usage Sys.argv.(0)) ;
  let divisions = 4 in
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
  Scendiv.divide !Options.scpath divisions;
  let fullscen = Scenario.load !Options.scpath in
  let fullscenlength = Scenario.length fullscen in
  let partscenlength = fullscenlength / divisions in
  Printf.printf "Length of scenarii (full/partial): %d/%d\n" fullscenlength
    partscenlength;

  let rec loop initpart cost k =
    if k * partscenlength >= !Options.horizon then cost else
      let sc = Scenario.load @@ !Options.scpath ^ (string_of_int k) in
      let module Env = struct
        let horizon = if (k + 1) * partscenlength < !Options.horizon then
            partscenlength - 1 else !Options.horizon
        let alpha = !Options.alpha
        let beta = !Options.beta
        let gamma = !Options.gamma
        let lambda = !Options.lambda
        let theta = !Options.theta
        let init = initpart
        let ctx = Scenario.ctx
        let sectors = Scenario.sectors
        let workload = Scenario.workload sc
      end in

      let module AirSupp = Airconf.Make(Env) in

      let module AstAir = struct
        type state = AirSupp.t
        type user_param = unit

        let initial_state _ = AirSupp.init
        let is_goal _ st = AirSupp.terminal st
        let next _ = AirSupp.produce
        let k _ st1 st2 = AirSupp.cost st2
        let h _ st = AirSupp.h st
        (* let h _ _ = 0. *)
        let do_at_insertion _ _ _ = ()
        let do_at_extraction _ _ _ _ = ()
      end in

      let module AstarForAir = A_star.Make(AstAir) in

      let path = AstarForAir.search () in
      let partialcost = List.fold_left (fun accu e ->
          accu +. AirSupp.cost e) 0. path
      and lastpart = AirSupp.get_partitions @@ List.hd path in
      loop lastpart (cost +. partialcost) (succ k) in
  let tot_cost = loop initial_partition 0. 0 in
  Printf.printf "%f\n" tot_cost
