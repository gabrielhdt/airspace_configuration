Arg.parse Options.speclist Options.anon_fun (Options.usage Sys.argv.(0)) ;;

let sc = Scenario.load !Options.scpath
let s15 = Util.Sset.add "1" (Util.Sset.add "5" Util.Sset.empty)
let s32 = Util.Sset.add "3" (Util.Sset.add "2" Util.Sset.empty)
let s4 = Util.Sset.add "4" Util.Sset.empty
let initial_partition = [
  (s15, [("d") ]) ; (s32, [("a") ]) ; (s4, [("s4") ])
]

module Env = struct
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
end

module Support = Airconf.Make(Env)

type node = Support.t
module NMap = Map.Make(struct type t = node let compare = compare end)
module NSet = Set.Make(struct type t = node let compare = compare end)

let produce = Support.produce

let cost = Support.cost

let terminal = Support.terminal

let argmin (set : NSet.t) = Support.init

let h = Support.h

let astar start =
  let rec loop open_set closed_set came_from g_score f_score =
    if open_set = NSet.empty then came_from else
      let current = argmin open_set in
      if terminal current then came_from else
        let u_open_set = NSet.remove current open_set
        and u_closed_set = NSet.add current closed_set in
        let neighbours = produce current in
        let u_came_from, u_g_score, u_f_score, uu_open_set =
          List.fold_left (fun acc neighbour ->
              let a_came_from, a_g_score, a_f_score, a_open_set = acc in
              let u_open_set = NSet.add neighbour a_open_set
              and curr_g_score = if NMap.mem current a_g_score then
                  NMap.find current a_g_score else infinity in
              let tentative_g_score = curr_g_score +.
                                      cost neighbour in
              if tentative_g_score >= NMap.find neighbour a_g_score then
                a_came_from, a_g_score, a_f_score, u_open_set
              else
                let u_came_from = NMap.add neighbour current a_came_from
                and u_g_score = NMap.add neighbour tentative_g_score a_g_score
                and u_f_score = NMap.add neighbour (tentative_g_score +.
                                                    h neighbour) a_f_score in
                u_came_from, u_g_score, u_f_score, u_open_set
            ) (came_from, g_score, f_score, u_open_set) neighbours in
        loop uu_open_set u_closed_set u_came_from u_g_score u_f_score
  in loop (NSet.add start NSet.empty) NSet.empty NMap.empty
    (NMap.add start 0. NMap.empty) (NMap.add start (h start) NMap.empty)

let () =
  let came_from = astar Support.init in ()
