let bestrew = ref 0.
let count = ref 0

let mctsinfo deplnodes reward =
  if reward > !bestrew then
    begin
      Printf.printf "\r" ;
      Printf.printf "%d improvements, " !count ;
      Printf.printf "best reward: %f, " reward ;
      Printf.printf "%d nodes deployed, " deplnodes ;
      Printf.printf "%!" ;
      bestrew := reward ; incr count
    end
