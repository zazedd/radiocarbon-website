type s = [ `Success | `Running | `Failed | `Waiting ]
type t = (string, s * string) Hashtbl.t

let main = "main"

let state : t =
  let h = Hashtbl.create 1 in
  Hashtbl.replace h main (`Waiting, "Waiting for pipeline to start");
  h

let set ~branch ~s = Hashtbl.replace state branch s
let get ~branch () = Hashtbl.find_opt state branch

let string_of_s = function
  | `Success -> "Success"
  | `Running -> "Running"
  | `Failed -> "Failed"
  | `Waiting -> "Waiting"

let to_strings (s, str) = (string_of_s s, str)
