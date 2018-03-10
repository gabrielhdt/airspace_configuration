(** Command line options handling *)

(** [usage s] outputs the usage string with [s] the program name *)
val usage : string -> string

(** List used for command line parsing *)
val speclist : (string * Arg.spec * string) list

(** Handles anonymous arguments (arguments without keywords) *)
val anon_fun : string -> 'a

val scpath : string ref
val tmax : int ref
val nsim : int ref
