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
    | Ok contribution as contrib ->
        Pages.contribution contribution
        |> Session.with_user request
        |> serve ~contrib ~extra_scripts "Radiocarbon Calibration"
    | Error msg -> Pages.error ~msg request |> serve msg

  let admin_view resource_path request =
    let extra_scripts =
      [
        Pages.Scripts.loader ~extra_action:"dropdowns ()" resource_path;
        Pages.Scripts.dropdown;
      ]
    in
    Pages.contributions_pending request
    |> serve ~extra_scripts "Radiocarbon Calibration"
end

(* ---------------------------------------------------------------------------- *)

module Post = struct
  let add request =
    let* id = Contributions_db.last_id () >|= string_of_int in
    let branch = "contrib_" ^ id in
    match%lwt Dream.form request with
    | `Ok [ ("foldername", folder_name); ("title", title) ] -> (
        Files_db.add_branch ~name:branch ~folder_name >>= function
        | Ok np ->
            Session.grab_session request |> Result.get_ok |> fun user ->
            Contributions_db.store id title user.email folder_name `Unmerged
            >>= fun contr ->
            Files_db.add_folder ~branch ~user_email:user.email
              ~in_where:[ inputs ] ~out_where:[ outputs ] ~name:folder_name np
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

  let update_aux id status request fn =
    Contributions_db.update id status >>= function
    | Ok _ -> fn
    | Error msg -> Pages.error ~msg request |> serve msg

  let update status request =
    let id = Dream.param request "id" in
    update_aux id status request
    @@ Dream.redirect request ("/dashboard" |> add_contribution_query id)

  let submit request = update `Submitted request
  let reject request = update `Rejected request

  let accept request =
    let id = Dream.param request "id" in
    update_aux id `Merged request
    @@ begin
         let from = "contrib_" ^ id in
         Contributions_db.get id
         >>= begin
               function
               | Ok contrib ->
                   Session.grab_session request |> Result.get_ok |> fun user ->
                   Files_db.fetch_all ()
                   >>= Files_db.merge_branches ~into:Files_db.branch ~from
                         ~name:contrib.folder_name ~user_email:user.email
                   >>= begin
                         function
                         | Ok _ ->
                             Dream.redirect request
                               ("/dashboard" |> add_contribution_query id)
                         | Error msg -> Pages.error ~msg request |> serve msg
                       end
               | Error msg -> Pages.error ~msg request |> serve msg
             end
       end
end

module Promises = struct
  let base_request ~fn request =
    Files_db.branch_names () >>= function
    | Ok contribs ->
        let* c = contribs in
        let ids =
          List.filter (String.starts_with ~prefix:"contrib_") c
          |> List.map (fun x -> Contributions.remove_contrib x)
        in
        Lwt_list.map_s
          (fun id ->
            Contributions_db.get id
            >>= begin
                  function
                  | Ok contrib -> contrib |> Lwt.return
                  | Error _ -> (
                      Logs.info (fun f ->
                          f "Contrib not in store, adding. %s@." id);
                      Contributions_db.store id
                        ("Contribution " ^ id
                       ^ ", restored from GitHub (database deleted)")
                        "noone@nowhere.com" ("contribution_" ^ id) `Restored
                      >|= function
                      | Ok contrib -> contrib
                      | Error _ -> assert false)
                end)
          ids
        >>= fun contribs ->
        Pages.Promises.contributions ~fn
          (List.sort Contributions.compare contribs)
        |> Session.with_user request |> respond
    | Error (`Msg _) -> Dream.empty `Bad_Request

  let from_user request =
    let fn (user : User.t_no_pw) lst =
      let open Contributions in
      lst
      |> List.filter (fun c -> c.email = user.email)
      |> List.filter (fun c ->
             match c.status with
             | `Unmerged | `Submitted | `Merged | `Rejected -> true
             | _ -> false)
    in
    base_request ~fn request

  let all_submitted_merged_rejected request =
    let fn _ lst =
      let open Contributions in
      lst
      |> List.filter (fun c ->
             match c.status with
             | `Submitted | `Merged | `Rejected -> true
             | _ -> false)
    in
    base_request ~fn request
end
