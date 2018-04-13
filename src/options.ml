let usage progname = "Usage: " ^ progname ^ " " ^
                     "-scenario string [-nsim int] [-tmax int] " ^
                     "[-alpha float] [-beta float] ..."

let anon_fun x = raise @@ Arg.Bad ("bad arg " ^ x)

let scpath = ref ""
let horizon = ref 5
let timeperstep = ref 2.
let nsteps = ref 1

let alpha = ref 1.
let beta = ref 1.
let gamma = ref 1.
let lambda = ref 1.
let theta = ref 1.
let expvexp = ref 2.

let verbose = ref false

let speclist = [
  ("-scenario", Arg.Set_string scpath,
   "Scenario to load and play") ;
  ("-horizon", Arg.Set_int horizon,
   "Time limit above which events are not seen (hence the name)") ;
  ("-timeperstep", Arg.Set_float timeperstep,
   "Time taken to choose next action/node/configuration") ;
  ("-nsteps", Arg.Set_int nsteps,
   "Length of the final path") ;
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
  ("-expvexp", Arg.Set_float expvexp,
   "Sets tradeoff between exploration and exploitation") ;
]
