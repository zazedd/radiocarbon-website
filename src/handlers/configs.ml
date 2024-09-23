open Lwt.Infix
open Lwt.Syntax
open Website_lib
open Util

module Get = struct
  let config resource_path request =
    let[@alert "-deprecated"] path_request = Dream.path request in
    let* _, contrib = branch_and_contrib_from_query request in
    match contrib with
    | (Some (Ok _) | None) as c -> begin
        let resource_path =
          resource_path_with_query resource_path path_request c
        in
        let extra_scripts =
          [
            Pages.Scripts.loader ~extra_action:"confirm ()" resource_path;
            Pages.Scripts.confirm;
          ]
        in
        Pages.config request
        |> serve ?contrib ~extra_scripts "Radiocarbon Calibration"
      end
    | Some (Error msg) -> Pages.error ~msg request |> serve msg

  let add request =
    let[@alert "-deprecated"] path = Dream.path request in
    let* branch, contrib = branch_and_contrib_from_query request in
    Files_db.fetch branch >>= fun np ->
    match contrib with
    | Some (Ok contribution) ->
        let* folder = Files_db.folder ~branch path np in
        Files_db.scripts ~branch scripts np >>= fun scripts ->
        Pages.add_config
          ~contrib_id:(contribution.id |> string_of_int)
          folder scripts request
        |> serve ?contrib "Radiocarbon Calibration"
    | None ->
        let* folder = Files_db.folder ~branch path np in
        Files_db.scripts ~branch scripts np >>= fun scripts ->
        Pages.add_config folder scripts request
        |> serve "Radiocarbon Calibration"
    | Some (Error msg) -> Pages.error ~msg request |> serve msg

  let edit resource_path request =
    let[@alert "-deprecated"] path = Dream.path request in
    let* _, contrib = branch_and_contrib_from_query request in
    match contrib with
    | Some (Ok contribution) ->
        let resource_path =
          get_resource_path resource_path path
          |> add_contribution_query (contribution.id |> string_of_int)
        in
        let extra_scripts = [ Pages.Scripts.loader resource_path ] in
        Pages.edit_config request
        |> serve ?contrib ~extra_scripts "Radiocarbon Calibration"
    | Some (Error msg) -> Pages.error ~msg request |> serve msg
    | None ->
        let resource_path = get_resource_path resource_path path in
        let extra_scripts = [ Pages.Scripts.loader resource_path ] in
        Pages.edit_config request
        |> serve ~extra_scripts "Radiocarbon Calibration"

  let edit_default resource_path request =
    let* _, contrib = branch_and_contrib_from_query request in
    match contrib with
    | Some (Ok _) ->
        let msg =
          "Do not change the default configuration. Create a custom one for \
           the contribution folder!"
        in
        Pages.error ~msg request |> serve msg
    | Some (Error msg) -> Pages.error ~msg request |> serve msg
    | None ->
        let extra_scripts = [ Pages.Scripts.loader resource_path ] in
        Pages.edit_config request
        |> serve ~extra_scripts "Radiocarbon Calibration"
end

module Post = struct
  let add request : Dream.response Lwt.t =
    let[@alert "-deprecated"] path = Dream.path request in
    let* branch, contrib = branch_and_contrib_from_query request in
    match contrib with
    | Some (Ok _) | None -> begin
        let path_str = String.concat "/" path in
        let folder_path = inputs :: path in
        match%lwt Dream.form request with
        | `Ok
            [
              ("column", column);
              ("confidence", confidence);
              ("curve", curve);
              ("script", script);
              ("step", step);
              ("time_left_bound", time_left_bound);
              ("time_right_bound", time_right_bound);
              ("value", value);
            ] -> begin
            let step = step |> int_of_string
            and confidence = confidence |> float_of_string
            and time_left_bound =
              if time_left_bound = "" then None
              else Some (time_left_bound |> int_of_string)
            and time_right_bound =
              if time_right_bound = "" then None
              else Some (time_right_bound |> int_of_string)
            and curve = if value = "default" then None else Some curve
            and column = if column = "" then None else Some column
            and value = if value = "" then None else Some value in
            Session.grab_session request |> Result.get_ok |> fun user ->
            Files_db.fetch branch
            >>= Files_db.add_config ~branch ~user_email:user.email
                  ~path:folder_path ~step ~confidence ~time_left_bound
                  ~time_right_bound ~curve ~column ~value ~script
            >>= function
            | Ok _ ->
                Dream.redirect request
                  ("/dashboard/configs/" ^ path_str
                  |> redirect_from_query request)
            | Error msg -> Pages.error ~msg request |> serve msg
          end
        | _ -> Dream.empty `Bad_Request
      end
    | Some (Error msg) -> Pages.error ~msg request |> serve msg

  let edit request : Dream.response Lwt.t =
    let[@alert "-deprecated"] path = Dream.path request in
    let* branch, contrib = branch_and_contrib_from_query request in
    match contrib with
    | Some (Ok _) | None -> begin
        let path_str = String.concat "/" path in
        let config_path = (inputs :: path) @ [ "config" ] in
        match%lwt Dream.form request with
        | `Ok
            [
              ("column", column);
              ("confidence", confidence);
              ("curve", curve);
              ("script", script);
              ("step", step);
              ("time_left_bound", time_left_bound);
              ("time_right_bound", time_right_bound);
              ("value", value);
            ] -> begin
            let step = step |> int_of_string
            and confidence = confidence |> float_of_string
            and time_left_bound =
              if time_left_bound = "default" then None
              else Some (time_left_bound |> int_of_string)
            and time_right_bound =
              if time_right_bound = "default" then None
              else Some (time_right_bound |> int_of_string)
            and curve = if value = "default" then None else Some curve
            and column = if column = "" then None else Some column
            and value = if value = "" then None else Some value in
            Session.grab_session request |> Result.get_ok |> fun user ->
            Files_db.fetch branch
            >>= Files_db.update_config ~branch ~user_email:user.email
                  ~path:config_path ~step ~confidence ~time_left_bound
                  ~time_right_bound ~curve ~column ~value ~script
            >>= function
            | Ok _ ->
                Dream.redirect request
                  ("/dashboard/configs/" ^ path_str
                  |> redirect_from_query request)
            | Error msg -> Pages.error ~msg request |> serve msg
          end
        | _ -> Dream.empty `Bad_Request
      end
    | Some (Error msg) -> Pages.error ~msg request |> serve msg

  let remove request =
    let[@alert "-deprecated"] path = Dream.path request in
    let* branch, contrib = branch_and_contrib_from_query request in
    match contrib with
    | Some (Ok _) | None -> begin
        let path_str = String.concat "/" path in
        let path = (inputs :: path) @ [ "config" ] in
        match%lwt Dream.form request with
        | `Ok [ ("hidden", "hidden") ] -> (
            Session.grab_session request |> Result.get_ok |> fun user ->
            Files_db.fetch branch
            >>= Files_db.remove_config ~branch ~user_email:user.email ~path
            >>= function
            | Ok () ->
                Dream.redirect request
                  ("/dashboard/configs/" ^ path_str
                  |> redirect_from_query request)
            | Error msg -> Pages.error ~msg request |> serve msg)
        | _ -> Dream.empty `Bad_Request
      end
    | Some (Error msg) -> Pages.error ~msg request |> serve msg
end

module Promises = struct
  let details request =
    let[@alert "-deprecated"] path = Dream.path request in
    let path = if List.hd path = "dashboard" then List.tl path else path in
    let* branch, contrib = branch_and_contrib_from_query request in
    Files_db.fetch branch >>= fun np ->
    let* folder = Files_db.folder ~branch path np in
    Files_db.config_file ~branch path np >>= fun config ->
    match contrib with
    | Some (Ok contrib) ->
        Pages.Promises.config_details ~contrib config folder
        |> Session.with_user request |> respond
    | None ->
        Pages.Promises.config_details config folder
        |> Session.with_user request |> respond
    | Some (Error msg) -> Pages.error ~msg request |> respond

  let edit request =
    let[@alert "-deprecated"] path = Dream.path request in
    let* branch, contrib = branch_and_contrib_from_query request in
    Files_db.fetch branch >>= fun np ->
    let* folder = Files_db.folder ~branch path np in
    Files_db.config_file ~branch path np >>= fun config ->
    Files_db.scripts ~branch scripts np >>= fun scripts ->
    match contrib with
    | Some (Ok contrib) ->
        Pages.Promises.config_edit_content
          ~contrib_id:(contrib.id |> string_of_int)
          folder scripts config request
        |> respond
    | None ->
        Pages.Promises.config_edit_content folder scripts config request
        |> respond
    | Some (Error msg) -> Pages.error ~msg request |> respond

  let edit_default request =
    (* contributions cannot change the default configuration *)
    let folder =
      Files.Folder.(Folder { name = ""; path = "." |> Fpath.v; files = [] })
    in
    Files_db.fetch Files_db.branch >>= fun np ->
    Files_db.config_file [ "config" ] np >>= fun config ->
    Files_db.scripts scripts np >>= fun scripts ->
    Pages.Promises.config_edit_content folder scripts config request |> respond
end
