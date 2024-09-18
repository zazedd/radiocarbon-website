open Lwt.Infix
open Website_lib
open Util

module Get = struct
  let users request =
    User_db.all_users () >>= Lwt.all >>= fun x ->
    Pages.users x |> Session.with_user request
    |> serve "Radiocarbon Dashboard - Users"

  let user request =
    Dream.param request "email" |> Base64.decode |> function
    | Error (`Msg msg) -> Pages.error request |> serve msg
    | Ok x -> (
        x |> User_db.user_get_email >>= fun x ->
        match x with
        | Error msg -> Pages.error ~msg request |> serve msg
        | Ok x -> Pages.user x request |> serve "Radiocarbon Dashboard - User")

  let edit request =
    Dream.param request "email" |> Base64.decode |> function
    | Error (`Msg msg) -> Pages.error request |> serve msg
    | Ok x -> (
        x |> User_db.user_get_email >>= fun x ->
        match x with
        | Error msg -> Pages.error ~msg request |> serve msg
        | Ok x ->
            Pages.edit_user x request |> serve "Radiocarbon Dashboard - User")

  let add request =
    Pages.add_user request |> serve "Radiocarbon Dashboard - Add User"
end

module Post = struct
  let add request : Dream.response Lwt.t =
    match%lwt Dream.form request with
    | `Ok
        [
          ("email", email);
          ("name", full_name);
          ("password", password);
          ("usertype", usertype);
        ] -> (
        let usertype = User.account_type_of_string usertype in
        User_db.user_store email password full_name usertype >>= function
        | Ok _ -> Dream.redirect request "/dashboard/users"
        | Error msg -> Pages.error ~msg request |> serve msg)
    | _ -> Dream.empty `Bad_Request

  let edit request : Dream.response Lwt.t =
    Dream.param request "email" |> Base64.decode |> function
    | Error (`Msg msg) -> Pages.error request |> serve msg
    | Ok x ->
        x |> User_db.user_get_email >>= fun x ->
        begin
          match x with
          | Error msg -> Pages.error ~msg request |> serve msg
          | Ok old_user -> begin
              match%lwt Dream.form request with
              | `Ok
                  [
                    ("email", email); ("name", full_name); ("usertype", usertype);
                  ] -> (
                  User_db.user_update old_user.email email full_name usertype
                  >>= function
                  | Ok _ -> Dream.redirect request "/dashboard/users"
                  | Error msg -> Pages.error ~msg request |> serve msg)
              | _ -> Dream.empty `Bad_Request
            end
        end

  let remove request : Dream.response Lwt.t =
    Dream.param request "email" |> Base64.decode |> function
    | Error (`Msg msg) -> Pages.error request |> serve msg
    | Ok x -> (
        match%lwt Dream.form request with
        | `Ok [ ("hidden", "hidden") ] ->
            x |> User_db.user_remove >>= fun x ->
            begin
              match x with
              | Error msg -> Pages.error ~msg request |> serve msg
              | Ok () -> Dream.redirect request "/dashboard/users"
            end
        | _ -> Dream.empty `Bad_Request)
end
