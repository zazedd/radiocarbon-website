open Lwt.Syntax
open Lwt.Infix
module Store = Irmin_git_unix.FS.KV (User)

let user_store email password full_name account_type :
    (User.t, string) result Lwt.t =
  let* repo = Store.Repo.v Db_config.Users.c in
  let* t = Store.main repo in
  let* b = Store.mem t [ email ] in
  if not b then
    let hashed_password = Password.hash ~password |> Base64.encode_string in
    let user = User.{ email; hashed_password; full_name; account_type } in
    (*TODO: change to Store.set *)
    let* () =
      Store.set_exn t ~info:(Db_config.Users.info "Updating...") [ email ] user
    in
    let+ v = Store.get t [ email ] in
    Ok v
  else Error "An user with the same email already exists!" |> Lwt.return

let user_update old_email email full_name usertype :
    (User.t, string) result Lwt.t =
  let* repo = Store.Repo.v Db_config.Users.c in
  let* t = Store.main repo in
  let* b = Store.mem t [ old_email ] in
  if b then
    let* u = Store.get t [ old_email ] in
    let account_type = User.account_type_of_string usertype in
    let* () =
      if old_email <> email then
        Store.remove_exn ~info:(fun () -> Store.Info.v 0L) t [ old_email ]
      else () |> Lwt.return
    in
    let user =
      User.
        { email; hashed_password = u.hashed_password; full_name; account_type }
    in
    (*TODO: change to Store.set *)
    let* () =
      Store.set_exn t ~info:(Db_config.Users.info "Updating...") [ email ] user
    in
    let+ v = Store.get t [ email ] in
    Ok v
  else Error "No user with that email exists. (Impossible)" |> Lwt.return

let user_get email password : (User.t, string) result Lwt.t =
  let* repo = Store.Repo.v Db_config.Users.c in
  let* t = Store.main repo in
  let hashed_password = Password.hash ~password |> Base64.encode_string in
  let* b = Store.mem t [ email ] in
  if b then
    let+ v = Store.get t [ email ] in
    if v.hashed_password = hashed_password then Ok v
    else Error "Incorrect account details. Please try again."
  else Error "User does not exist." |> Lwt.return

let user_get_email email : (User.t_no_pw, string) result Lwt.t =
  let* repo = Store.Repo.v Db_config.Users.c in
  let* t = Store.main repo in
  let* b = Store.mem t [ email ] in
  if b then
    let+ v = Store.get t [ email ] in
    Ok (v |> User.user_no_pw)
  else Error "User does not exist." |> Lwt.return

let user_remove email : (unit, string) result Lwt.t =
  let* repo = Store.Repo.v Db_config.Users.c in
  let* t = Store.main repo in
  let* b = Store.mem t [ email ] in
  if b then
    let+ u = Store.remove_exn ~info:(fun () -> Store.Info.v 0L) t [ email ] in
    Ok u
  else Error "User does not exist." |> Lwt.return

let all_users () =
  let* repo = Store.Repo.v Db_config.Users.c in
  let* t = Store.main repo in
  let* tree = Store.tree t in
  let+ x = Store.Tree.list tree [] in
  let names = List.map fst x in
  List.iter (Format.printf "-> %s@.") names;
  List.map (fun x -> Store.get t [ x ] >|= User.user_no_pw) names
