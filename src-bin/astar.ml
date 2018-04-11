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

let gnodecount = ref 0
let glabel = ref 0

module Support = Airconf.Make(Env)

type node = int * Support.t
module NMap = Map.Make(struct type t = node let compare = compare end)
module NSet = Set.Make(struct type t = node let compare = compare end)

let produce node =
  let init_label = fst node
  and new_states = Support.produce @@ snd node in
  List.mapi (fun _ elt ->
      incr glabel ; (init_label + !glabel, elt)) new_states

let cost n = Support.cost @@ snd n

let terminal n = Support.terminal @@ snd n

let argmin (defset : NSet.t) f =
  let chosen = NSet.choose defset in
  let minelt, minscore = NSet.fold (fun elt acc ->
      let minelt, minscore = acc
      and eltscore = f elt in
      if eltscore < minscore then elt, eltscore else minelt, minscore)
      defset (chosen, f chosen)
  in minelt

let h n = Support.h @@ snd n
(*let h node = 0.*)

let reconstruct_path came_from current =
  let rec loop total_path currnode =
    if NMap.mem currnode came_from then
      let next = NMap.find currnode came_from in
      loop (next :: total_path) next
    else total_path in
  loop [current] current

let astar start =
  let rec loop open_set closed_set came_from g_score f_score =
    if open_set = NSet.empty then failwith "no path" else
      let current = argmin open_set (fun elt ->
          if NMap.mem elt f_score then NMap.find elt f_score else infinity) in
      incr gnodecount ;
      if terminal current then reconstruct_path came_from current else
        let u_open_set = NSet.remove current open_set
        and u_closed_set = NSet.add current closed_set in
        let neighbours = produce current in
        let u_came_from, u_g_score, u_f_score, uu_open_set =
          List.fold_left (fun acc neighbour ->
              if NSet.mem neighbour u_closed_set then acc else
                let a_came_from, a_g_score, a_f_score, a_open_set = acc in
                let u_open_set = NSet.add neighbour a_open_set
                and curr_g_score = if NMap.mem current a_g_score then
                    NMap.find current a_g_score else infinity
                and neigh_g_score = if NMap.mem neighbour a_g_score then
                    NMap.find neighbour a_g_score else infinity in
                let tentative_g_score = curr_g_score +. cost neighbour in
                if tentative_g_score >= neigh_g_score then
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
  let path = astar (0, Support.init) in
  let pathcost = List.fold_left (fun acc elt ->
      acc +. (Support.cost @@ snd elt)) 0. path in
  let n = List.length path in
  Printf.printf "path cost : %f path length %d \n" (pathcost) n;
  Printf.printf "Node count: %d\n" !gnodecount
