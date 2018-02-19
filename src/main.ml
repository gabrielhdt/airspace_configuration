let usage = "Usage: " ^ Sys.argv.(0) ^ " " ^
            "[-scenario string]"

let scpath = ref ""

let speclist = [
  ("-scenario", Arg.Set_string scpath,
   "scenario to load and play") ;
]
let () =
  Arg.parse speclist (fun x -> raise (Arg.Bad ("bad argument: " ^ x))) usage ;
  let sc = Scenario.load !scpath in
  let module Wl = struct
    let f = Scenario.f sc
  end
  in
  let module Support = Airconf.Make(Wl) in
  let s01 = (Util.Sset.add "1" Util.Sset.empty) in
  let s02 = (Util.Sset.add "2" Util.Sset.empty) in
  let s03 = (Util.Sset.add "3" Util.Sset.empty) in
  let s04 = (Util.Sset.add "4" Util.Sset.empty) in
  let s05 = (Util.Sset.add "5" Util.Sset.empty) in
  let initial_partition = [(s01, ["s1"]); (s02, ["s2"]); (s03, ["s3"]);
                           (s04, ["s4"]); (s05, ["s5"])] in
  let module Airmcts = Mcts.Make(Support) in
  let root = Airmcts.make_node (Support.make_root initial_partition) in

  let b_path = (Airmcts.best_path_robust root 7) in
  Printf.printf "%d\n" (List.length b_path);

  (*
  Partitions.print_partitions
    (List.map (fun s -> Support.get_partitions s)
       (List.map (fun tree -> Airmcts.get_state tree) b_path
       )
    )
     *)
