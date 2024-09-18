open Lwt.Syntax
open Lwt.Infix
module Store = Irmin_git_unix.FS.KV (Contributions)

let store id title email folder_name status :
    (Contributions.t, string) result Lwt.t =
  let* repo = Store.Repo.v Db_config.Contributions.c in
  let* t = Store.main repo in
  let* b = Store.mem t [ id ] in
  if not b then
    let contr =
      Contributions.{ id = int_of_string id; title; email; folder_name; status }
    in
    let* () =
      Store.set_exn t
        ~info:(Db_config.Contributions.info "Adding...")
        [ id ] contr
    in
    let+ v = Store.get t [ id ] in
    Ok v
  else Error "A contribution with that ID already exists!" |> Lwt.return

let update id new_title : (Contributions.t, string) result Lwt.t =
  let* repo = Store.Repo.v Db_config.Contributions.c in
  let* t = Store.main repo in
  let* b = Store.mem t [ id ] in
  if b then
    let* u = Store.get t [ id ] in
    let contr = { u with title = new_title } in
    let* () =
      Store.set_exn t ~info:(Db_config.Users.info "Updating...") [ id ] contr
    in
    let+ v = Store.get t [ id ] in
    Ok v
  else Error "Contribution does not exist. (Impossible)" |> Lwt.return

let get id : (Contributions.t, string) result Lwt.t =
  let* repo = Store.Repo.v Db_config.Contributions.c in
  let* t = Store.main repo in
  let* b = Store.mem t [ id ] in
  if b then Store.get t [ id ] >|= fun contr -> Ok contr
  else Error "Contribution does not exist." |> Lwt.return

let last_id () : int Lwt.t =
  let* repo = Store.Repo.v Db_config.Contributions.c in
  let* t = Store.main repo in
  let+ lst = Store.list t [] in
  List.fold_left
    (fun acc (id, _) ->
      Format.printf "%s@." id;
      max acc (id |> int_of_string))
    0 lst
  + 1
