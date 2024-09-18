open Lwt.Infix
open Lwt.Syntax
open Website_lib
open Util

module Get = struct
  let add request =
    Pages.add_contribution request |> serve "Radiocarbon Calibration"

  let contrib resource_path request =
    let id = Dream.param request "id" in
    let resource_path = resource_path ^ id in
    let extra_scripts =
      [
        Pages.Scripts.loader ~extra_action:"dropdowns ()" resource_path;
        Pages.Scripts.dropdown;
      ]
    in
    Contributions_db.get id >>= function
    | Ok contrib ->
        Pages.contribution contrib request
        |> serve ~contrib ~extra_scripts "Radiocarbon Calibration"
    | Error msg -> Pages.error ~msg request |> serve msg
end

(* ---------------------------------------------------------------------------- *)

module Post = struct
  let add request =
    let* id = Contributions_db.last_id () >|= string_of_int in
    let branch = "contrib_" ^ id in
    match%lwt Dream.form request with
    | `Ok [ ("foldername", name); ("title", title) ] -> (
        Files_db.add_branch ~name:branch >>= function
        | Ok np ->
            Session.grab_session request |> Result.get_ok |> fun user ->
            Contributions_db.store id title user.email name `Unmerged
            >>= fun contr ->
            Files_db.add_folder ~branch ~user_email:user.email
              ~in_where:[ inputs ] ~out_where:[ outputs ] ~name np
            >>= begin
                  function
                  | Ok () -> begin
                      match contr with
                      | Ok _ ->
                          Dream.redirect request
                            ("/dashboard" |> add_contribution_query id)
                      | Error msg -> Pages.error ~msg request |> serve msg
                    end
                  | Error msg -> Pages.error ~msg request |> serve msg
                end
        | Error msg -> Pages.error ~msg request |> serve msg)
    | _ -> Dream.empty `Bad_Request
end
