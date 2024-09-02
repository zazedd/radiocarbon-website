open Irmin

type t = {
  email : string;
  hashed_password : string;
  full_name : string;
  account_type : [ `Admin | `User | `ElevatedUser ];
}
[@@deriving irmin]

type t_no_pw = {
  email : string;
  full_name : string;
  account_type : [ `Admin | `User | `ElevatedUser ];
}
[@@deriving irmin]

let user_no_pw (v : t) : t_no_pw =
  { full_name = v.full_name; email = v.email; account_type = v.account_type }

let pre_hash = Type.(unstage (pre_hash t))

(* Manually add a prefix to default contents, in order to prevent hash
    collision between contents and nodes or commits (see
    https://github.com/mirage/irmin/issues/1304). *)
let pre_hash_prefixed x f =
  f "B";
  pre_hash x f

let t = Type.(like t ~pre_hash:pre_hash_prefixed)
let merge = Merge.idempotent Type.(option t)

let account_type_to_string = function
  | `Admin -> "Admin"
  | `User -> "User"
  | `ElevatedUser -> "Elevated User"

let account_type_of_string = function
  | "Admin" -> `Admin
  | "User" -> `User
  | "ElevatedUser" -> `ElevatedUser
  | _ -> failwith "Invalid Usertype"

let marshal (t : t_no_pw) =
  let json =
    `Assoc
      [
        ("email", `String t.email);
        ("full_name", `String t.full_name);
        ("account_type", `String (t.account_type |> account_type_to_string));
      ]
  in
  Yojson.to_string json

let unmarshal s =
  match Yojson.Safe.from_string s with
  | `Assoc
      [
        ("email", `String email);
        ("full_name", `String full_name);
        ("account_type", `String at);
      ] ->
      let account_type = account_type_of_string at in
      { email; full_name; account_type }
  | _ -> failwith "Failed to unmarshal files"
