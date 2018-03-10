(** Command line options handling *)

(** [usage s] outputs the usage string with [s] the program name *)
val usage : string -> string

(** List used by {function:Arg.parse} *)
val speclist : (string * Arg.spec * string) list

(** Handles anonymous arguments (see {!module:Arg}) *)
val anon_fun : string -> 'a

val scpath : string ref
val tmax : int ref
val nsim : int ref
