let dummy () =
  Mcts.best_child

let () =
  let scpath = Sys.argv.(1) in
  let sc = Scenario.load scpath in
  let f = Scenario.f sc in
  ()
