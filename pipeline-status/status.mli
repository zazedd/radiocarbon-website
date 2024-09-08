type s = [ `Complete | `Running | `Failed | `Waiting ]
type t = s * string

val set : s:t -> unit
val get : unit -> t
