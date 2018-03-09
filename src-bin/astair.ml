
(*
module type AstarAirconf = sig
  type state
  type user_param

  val is_goal: user_param -> state -> bool
  val next: user_param -> state -> state list
  val k: user_param -> state -> state -> float
  val h: user_param -> state -> float

  val do_at_insertion:
      user_param -> state -> state -> unit
  val do_at_extraction:
      user_param -> (prio, state) Pqueue.t -> state Memory.t -> state -> unit
end
   *)

let usage = "Usage: " ^ Sys.argv.(0) ^ " " ^
            "-scenario string [-tmax int]"

let scpath = ref ""
let tmax = ref 5
let balance = ref 0.5

let speclist = [
  ("-scenario", Arg.Set_string scpath,
   "Scenario to load and play") ;
  ("-tmax", Arg.Set_int tmax,
   "Depth of the tree") ;
  ("-balance", Arg.Set_float balance,
   "Balance between the configuration cost and the transition cost")
]
let () =
  Arg.parse speclist (fun x -> raise @@ Arg.Bad ("bad arg " ^ x)) usage ;
  let sc = Scenario.load !scpath in
  let module Env = struct
    let tmax = !tmax
    let workload = Scenario.workload sc
  end in
  let module AirSupp = Airconf.Make(Env) in
  (* Inital state *)
  let s15 = Util.Sset.add "1" (Util.Sset.add "5" Util.Sset.empty)
  and s32 = Util.Sset.add "3" (Util.Sset.add "2" Util.Sset.empty)
  and s4 = Util.Sset.add "4" Util.Sset.empty in
  let initial_partition = [(s15, ["d"]); (s32, ["a"]); (s4, ["s4"])] in
  let init_state = AirSupp.make_root initial_partition in

  let module AstAir = struct
    type state = AirSupp.t
    type user_param = unit

    let initial_state _ = init_state
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
