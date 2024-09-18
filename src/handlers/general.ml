open Lwt.Infix
open Lwt.Syntax
open Website_lib
open Util

module Get = struct
  let mainpage request =
    Pages.mainpage request |> serve "Radiocarbon Calibration"

  let login request =
    Pages.login request |> serve "Radiocarbon Calibration - Login"

  let register request =
    Pages.register request |> serve "Radiocarbon Calibration - Register"

  let dashboard files_resource_path contributions_resource_path request =
    let* _, contrib = branch_and_contrib_from_query request in
    let extra_scripts =
      [
        Pages.Scripts.dropdown;
        Pages.Scripts.loader ~content_div:"contribution_content"
          contributions_resource_path;
      ]
    in
    match contrib with
    | Some (Ok contrib) ->
        let contrib_files_resource_path =
          files_resource_path
          |> add_contribution_query (contrib.id |> string_of_int)
        in
        let extra_scripts =
          Pages.Scripts.loader ~extra_action:"dropdowns ()"
            contrib_files_resource_path
          :: extra_scripts
        in
        Pages.contribution contrib request
        |> serve ~contrib ~extra_scripts "Radiocarbon Dashboard"
    | Some (Error msg) -> Pages.error ~msg request |> serve msg
    | None ->
        let extra_scripts =
          Pages.Scripts.loader ~extra_action:"dropdowns ()" files_resource_path
          :: extra_scripts
        in
        Pages.dashboard |> Session.with_user request
        |> serve ~extra_scripts "Radiocarbon Dashboard"
end

module Post = struct
  let register request : Dream.response Lwt.t =
    match%lwt Dream.form request with
    | `Ok [ ("email", email); ("name", full_name); ("password", passwd) ] -> (
        User_db.user_store email passwd full_name `Admin >>= function
        | Ok user ->
            let unopw = User.user_no_pw user |> User.marshal in
            let%lwt () = Dream.invalidate_session request in
            let%lwt () = Dream.set_session_field request "user" unopw in
            Dream.redirect request "/dashboard"
        | Error msg -> Pages.error ~msg request |> serve msg)
    | _ -> Dream.empty `Bad_Request

  let login request : Dream.response Lwt.t =
    match%lwt Dream.form request with
    | `Ok [ ("email", email); ("password", passwd) ] -> (
        User_db.user_get email passwd >>= function
        | Ok user ->
            let unopw = User.user_no_pw user |> User.marshal in
            let%lwt () = Dream.invalidate_session request in
            let%lwt () = Dream.set_session_field request "user" unopw in
            Dream.redirect request "/dashboard"
        | Error msg -> Pages.error ~msg request |> serve msg)
    | _ -> Dream.empty `Bad_Request

  let logout request : Dream.response Lwt.t =
    let%lwt () = Dream.invalidate_session request in
    Dream.redirect request "/"
end

module Promises = struct
  let dashboard_files request =
    let* branch, contrib = branch_and_contrib_from_query request in
    let open Contributions in
    Files_db.fetch branch >>= fun np ->
    match contrib with
    | Some contrib -> begin
        match contrib with
        | Ok contrib ->
            Files_db.folder ~branch [ contrib.folder_name ] np >>= fun files ->
            let files =
              Files.Folder.Folder
                { name = inputs; path = inputs |> Fpath.v; files = [ files ] }
            in
            Pages.Promises.dashboard_files
              ~contrib:(contrib.id |> string_of_int)
              files request
            |> respond
        | Error msg -> Pages.error ~msg request |> serve msg
      end
    | None ->
        Files_db.all_files ~branch np >>= fun files ->
        Pages.Promises.dashboard_files files request |> respond

  let contribution_branches request =
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
        Pages.Promises.contributions
          (List.sort Contributions.compare contribs)
          request
        |> respond
    | Error (`Msg _) -> Dream.empty `Bad_Request

  let pipeline_status_topbar request =
    let* branch, _ = branch_and_contrib_from_query request in
    [
      Status.get ~branch () |> Option.get
      |> Pages.Promises.pipeline_topbar_content;
    ]
    |> respond

  let pipeline_status_popup request =
    let* branch, _ = branch_and_contrib_from_query request in
    [
      Status.get ~branch () |> Option.get
      |> Pages.Promises.pipeline_popup_content ~branch;
    ]
    |> respond
end
