type s = [ `Complete | `Running | `Failed | `Waiting ]
type t = s * string

let state : t ref = (`Waiting, "Waiting for pipeline to start") |> ref
let set ~s = state := s
let get () = !state
