type s = [ `Success | `Running | `Failed | `Waiting ]
type t = (string, s * string) Hashtbl.t

val set : branch:string -> s:s * string -> unit
val get : branch:string -> unit -> (s * string) option
val to_strings : s * string -> string * string
