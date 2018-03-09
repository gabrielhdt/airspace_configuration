let usage progname = "Usage: " ^ progname ^ " " ^
            "-scenario string [-nsim int] [-tmax int]"

let scpath = ref ""
let tmax = ref 7
let nsim = ref 10

let speclist = [
  ("-scenario", Arg.Set_string scpath,
   "Scenario to load and play") ;
  ("-nsim", Arg.Set_int nsim,
   "Number of simulations carried out for each selection") ;
  ("-tmax", Arg.Set_int tmax,
   "Depth of the tree")
]
