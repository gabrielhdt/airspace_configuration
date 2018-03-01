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
      user_param -> (prio,state) Pqueue.t -> state Memory.t -> state -> unit
end

let usage = "Usage: " ^ Sys.argv.(0) ^ " " ^
            "-scenario string [-tmax int]"

let scpath = ref ""
let tmax = ref 5

let speclist = [
  ("-scenario", Arg.Set_string scpath,
   "Scenario to load and play") ;
  ("-tmax", Arg.Set_int tmax,
   "Depth of the tree") ;
]

let () =
  let sc = Scenario.load !scpath in
  let module Wl = struct
    let tmax = !tmax
    let f = Scenario.f sc
  end in
  let module AirSupp = Airconf.Make(Wl) in
  let module AstAir = struct
    type state = AirSupp.t
    type user_param = unit
    let is_goal _ st = AirSupp.terminal st
    let next = AirSupp.produce
    let k _ st1 st2 = AirSupp.reward st2 +. 10. *. if st2 = st1 then 1. else 0.
    let h _ st = AirSupp.reward st
    let do_at_insertion _ _ _ = ()
    let do_at_extraction _ _ _ _ = ()
  end in
  let module AstarForAir = A_star.Make(AstAir) in
  let final_state = AstarForAir.search ()
