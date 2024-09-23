open Irmin
module C = struct end

type t = {
  id : int;
  title : string;
  email : string;
  folder_name : string; (* each contribution pushes to a designated folder *)
  status : [ `Unmerged | `Merged | `Submitted | `Rejected | `Restored ];
      (* could be in a normal state (unmerged or merged)
         or restored from GitHub. (deleted from database)

         Submitted means that the contributor has submitted
         the contribution to review

         If restored, email and title are lost, and it is
         effectively unmerged as branches get deleted
      *)
}
[@@deriving irmin]

let remove_contrib t =
  let l = String.length "contrib_" in
  String.sub t l (String.length t - l)

let compare t1 t2 = compare t2.id t1.id (* newer first *)

let string_of_status = function
  | `Unmerged -> "Unmerged"
  | `Merged -> "Merged"
  | `Submitted -> "Submitted"
  | `Restored -> "Restored"

let status_of_string = function
  | "Unmerged" -> `Unmerged
  | "Merged" -> `Merged
  | "Submitted" -> `Submitted
  | "Restored" -> `Restored
  | _ -> assert false

let pre_hash = Type.(unstage (pre_hash t))

(* Manually add a prefix to default contents, in order to prevent hash
    collision between contents and nodes or commits (see
    https://github.com/mirage/irmin/issues/1304). *)
let pre_hash_prefixed x f =
  f "B";
  pre_hash x f

let t = Type.(like t ~pre_hash:pre_hash_prefixed)
let merge = Merge.idempotent Type.(option t)
