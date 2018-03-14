let usage progname = "Usage: " ^ progname ^ " " ^
                     "-scenario string [-nsim int] [-tmax int] " ^
                     "[-alpha float] [-beta float] ..."

let anon_fun x = raise @@ Arg.Bad ("bad arg " ^ x)

let scpath = ref ""
let tmax = ref 7
let nsim = ref 10
let alpha = ref 1000.
let beta = ref 10.
let gamma = ref 100.
let lambda = ref 100.
let theta = ref 1.
let verbose = ref false

let speclist = [
  ("-scenario", Arg.Set_string scpath,
   "Scenario to load and play") ;
  ("-nsim", Arg.Set_int nsim,
   "Number of simulations carried out for each selection") ;
  ("-tmax", Arg.Set_int tmax,
   "Depth of the tree") ;
  ("-alpha", Arg.Set_float alpha,
   "Sets how much overload is taken into account") ;
  ("-beta", Arg.Set_float beta,
   "Sets how much normal workload is taken into account") ;
  ("-gamma", Arg.Set_float gamma,
   "Sets how much low workload is taken into account") ;
  ("-lambda", Arg.Set_float lambda,
   "Sets how much the number of open positions is taken into account") ;
  ("-theta", Arg.Set_float theta,
   "Sets how much the cost of transitions is taken into account") ;
  ("-verbose", Arg.Set verbose,
   "Verbose mode: print final path") ;
  ("-v", Arg.Set verbose,
   "Verbose mode: print final path") ;
]
