(** Command line options handling *)

(** [usage s] outputs the usage string with [s] the program name *)
val usage : string -> string

(** List used for command line parsing *)
val speclist : (string * Arg.spec * string) list

(** Handles anonymous arguments (arguments without keywords) *)
val anon_fun : string -> 'a

val scpath : string ref
val maxsearch : int ref
val maxtime : float ref
val alpha : float ref
val beta : float ref
val gamma : float ref
val lambda : float ref
val theta : float ref
val verbose : bool ref
