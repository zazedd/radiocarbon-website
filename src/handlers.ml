open Lwt.Infix
open Lwt.Syntax
open Website_lib

let tyxml_list_to_string html =
  List.map (Format.asprintf "%a" (Tyxml.Html.pp_elt ())) html
  |> String.concat ""

let serve ?(extra_scripts = [ Tyxml.Html.(script (txt "")) ]) title page =
  Dream.html @@ (Pages.base title page extra_scripts |> tyxml_list_to_string)

let inputs = "inputs"
let outputs = "outputs"
let scripts = "scripts"

module Get = struct
  let mainpage request =
    Pages.mainpage request |> serve "Radiocarbon Calibration"

  let login request =
    Pages.login request |> serve "Radiocarbon Calibration - Login"

  let register request =
    Pages.register request |> serve "Radiocarbon Calibration - Register"

  let dashboard path request =
    let extra_scripts =
      [
        Pages.Scripts.dropdown;
        Pages.Scripts.loader ~extra_action:"dropdowns ()" path;
      ]
    in
    Pages.dashboard |> Session.with_user request
    |> serve ~extra_scripts "Radiocarbon Dashboard"

  module Users = struct
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
          | Ok x -> Pages.user x request |> serve "Radiocarbon Dashboard - User"
          )

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

  module Configs = struct
    let config path request =
      let before = Dream.param request "path" in
      let[@alert "-deprecated"] path_r = Dream.path request in
      let path =
        if List.hd path_r = "dashboard" then path ^ "/" ^ before
        else path ^ "/" ^ (before :: path_r |> String.concat "/")
      in
      let extra_scripts =
        [
          Pages.Scripts.loader ~extra_action:"confirm ()" path;
          Pages.Scripts.confirm;
        ]
      in
      Pages.config request |> serve ~extra_scripts "Radiocarbon Calibration"

    let add request =
      let[@alert "-deprecated"] path = Dream.path request in
      Files_db.fetch Files_db.branch >>= fun np ->
      let* folder = Files_db.folder path np in
      Files_db.scripts scripts np >>= fun scripts ->
      Pages.add_config folder scripts request |> serve "Radiocarbon Calibration"

    let edit path request =
      let[@alert "-deprecated"] path =
        path ^ "/" ^ (Dream.path request |> String.concat "/")
      in
      let extra_scripts = [ Pages.Scripts.loader path ] in
      Pages.edit_config request
      |> serve ~extra_scripts "Radiocarbon Calibration"

    let edit_default path request =
      let extra_scripts = [ Pages.Scripts.loader path ] in
      Pages.edit_config request
      |> serve ~extra_scripts "Radiocarbon Calibration"
  end

  module Files = struct
    let file path request =
      let before = Dream.param request "path" in
      let[@alert "-deprecated"] path_r = Dream.path request in
      let path =
        if List.hd path_r = "dashboard" then path ^ "/" ^ before
        else path ^ "/" ^ (before :: path_r |> String.concat "/")
      in
      let extra_scripts =
        [
          Pages.Scripts.loader ~extra_action:"confirm ()" path;
          Pages.Scripts.confirm;
        ]
      in
      Pages.file request |> serve ~extra_scripts "Radiocarbon Calibration"

    let add request =
      let[@alert "-deprecated"] folder_path =
        Dream.path request |> String.concat "/"
      in
      Pages.add_file folder_path request |> serve "Radiocarbon Calibration"

    let add_folder request =
      let[@alert "-deprecated"] path =
        Dream.path request |> String.concat "/"
      in
      Pages.add_folder path request |> serve "Radiocarbon Calibration"

    let rename_folder request =
      let[@alert "-deprecated"] path = Dream.path request in
      let old_name = path |> List.rev |> List.hd in
      let path = path |> String.concat "/" in
      Pages.rename_folder old_name path request
      |> serve "Radiocarbon Calibration"
  end
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

  module Users = struct
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
                      ("email", email);
                      ("name", full_name);
                      ("usertype", usertype);
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

  module Configs = struct
    let add request : Dream.response Lwt.t =
      let[@alert "-deprecated"] path = Dream.path request in
      let path_str = String.concat "/" path in
      let folder_path = inputs :: path in
      match%lwt Dream.form request with
      | `Ok
          [
            ("column", column);
            ("confidence", confidence);
            ("script", script);
            ("step", step);
            ("value", value);
          ] -> (
          let step = step |> int_of_string
          and confidence = confidence |> float_of_string
          and column = if column = "" then None else Some column
          and value = if value = "" then None else Some value in
          Session.grab_session request |> Result.get_ok |> fun user ->
          Files_db.fetch Files_db.branch
          >>= Files_db.add_config ~user_email:user.email ~path:folder_path ~step
                ~confidence ~column ~value ~script
          >>= function
          | Ok _ -> Dream.redirect request ("/dashboard/configs/" ^ path_str)
          | Error msg -> Pages.error ~msg request |> serve msg)
      | _ -> Dream.empty `Bad_Request

    let edit request : Dream.response Lwt.t =
      let[@alert "-deprecated"] path = Dream.path request in
      let path_str = String.concat "/" path in
      let config_path = (inputs :: path) @ [ "config" ] in
      match%lwt Dream.form request with
      | `Ok
          [
            ("column", column);
            ("confidence", confidence);
            ("script", script);
            ("step", step);
            ("value", value);
          ] -> (
          let step = step |> int_of_string
          and confidence = confidence |> float_of_string
          and column = if column = "" then None else Some column
          and value = if value = "" then None else Some value in
          Session.grab_session request |> Result.get_ok |> fun user ->
          Files_db.fetch Files_db.branch
          >>= Files_db.update_config ~user_email:user.email ~path:config_path
                ~step ~confidence ~column ~value ~script
          >>= function
          | Ok _ -> Dream.redirect request ("/dashboard/configs/" ^ path_str)
          | Error msg -> Pages.error ~msg request |> serve msg)
      | _ -> Dream.empty `Bad_Request

    let edit_default request : Dream.response Lwt.t =
      let config_path = [ "config" ] in
      match%lwt Dream.form request with
      | `Ok
          [
            ("column", column);
            ("confidence", confidence);
            ("script", script);
            ("step", step);
            ("value", value);
          ] -> (
          let step = step |> int_of_string
          and confidence = confidence |> float_of_string
          and column = if column = "" then None else Some column
          and value = if value = "" then None else Some value in
          Session.grab_session request |> Result.get_ok |> fun user ->
          Files_db.fetch Files_db.branch
          >>= Files_db.update_config ~user_email:user.email ~path:config_path
                ~step ~confidence ~column ~value ~script
          >>= function
          | Ok _ -> Dream.redirect request "/dashboard"
          | Error msg -> Pages.error ~msg request |> serve msg)
      | _ -> Dream.empty `Bad_Request

    let remove request =
      let[@alert "-deprecated"] path = Dream.path request in
      let path_str = String.concat "/" path in
      let path = (inputs :: path) @ [ "config" ] in
      match%lwt Dream.form request with
      | `Ok [ ("hidden", "hidden") ] -> (
          Session.grab_session request |> Result.get_ok |> fun user ->
          Files_db.fetch Files_db.branch
          >>= Files_db.remove_config ~user_email:user.email ~path
          >>= function
          | Ok () -> Dream.redirect request ("/dashboard/configs/" ^ path_str)
          | Error msg -> Pages.error ~msg request |> serve msg)
      | _ -> Dream.empty `Bad_Request
  end

  module Files = struct
    let add request =
      let[@alert "-deprecated"] path = Dream.path request in
      let path =
        if List.hd path = "dashboard" then [ inputs ] else inputs :: path
      in
      match%lwt Dream.multipart request with
      | `Ok [ ("file", [ (name, content) ]) ] -> (
          match name with
          | Some name -> begin
              Session.grab_session request |> Result.get_ok |> fun user ->
              Files_db.fetch Files_db.branch
              >>= Files_db.add_file ~user_email:user.email ~path ~name ~content
              >>= function
              | Ok () -> Dream.redirect request "/dashboard"
              | Error msg -> Pages.error ~msg request |> serve msg
            end
          | None ->
              let msg = "File has no name" in
              Pages.error ~msg request |> serve msg)
      | _ -> Dream.empty `Bad_Request

    let remove request =
      let[@alert "-deprecated"] path = Dream.path request in
      let name, in_where, out_where =
        let name = Files.last_element path |> Option.value ~default:"" in
        let path = path |> List.rev |> List.tl |> List.rev in
        (name, [ inputs ] @ path, [ outputs ] @ path)
      in
      match%lwt Dream.form request with
      | `Ok [ ("hidden", "hidden") ] -> (
          Session.grab_session request |> Result.get_ok |> fun user ->
          Files_db.fetch Files_db.branch
          >>= Files_db.remove_file ~user_email:user.email ~in_where ~out_where
                ~name
          >>= function
          | Ok () -> Dream.redirect request "/dashboard"
          | Error msg -> Pages.error ~msg request |> serve msg)
      | _ -> Dream.empty `Bad_Request

    let add_folder request =
      let[@alert "-deprecated"] path = inputs :: Dream.path request in
      match%lwt Dream.form request with
      | `Ok [ ("name", name) ] -> (
          Session.grab_session request |> Result.get_ok |> fun user ->
          Files_db.fetch Files_db.branch
          >>= Files_db.add_folder ~user_email:user.email ~path ~name
          >>= function
          | Ok () -> Dream.redirect request "/dashboard"
          | Error msg -> Pages.error ~msg request |> serve msg)
      | _ -> Dream.empty `Bad_Request

    let rename_folder request =
      let[@alert "-deprecated"] path = Dream.path request in
      let rev_path = path |> List.rev in
      let old_name = rev_path |> List.hd in
      let path = inputs :: (rev_path |> List.tl |> List.rev) in
      match%lwt Dream.form request with
      | `Ok [ ("name", new_name) ] -> (
          Session.grab_session request |> Result.get_ok |> fun user ->
          Files_db.fetch Files_db.branch
          >>= Files_db.rename_folder ~old_name ~new_name ~user_email:user.email
                ~path
          >>= function
          | Ok () -> Dream.redirect request "/dashboard"
          | Error msg -> Pages.error ~msg request |> serve msg)
      | _ -> Dream.empty `Bad_Request

    let remove_folder request =
      let[@alert "-deprecated"] path = inputs :: Dream.path request in
      match%lwt Dream.form request with
      | `Ok [ ("hidden", "hidden") ] -> (
          Session.grab_session request |> Result.get_ok |> fun user ->
          Files_db.fetch Files_db.branch
          >>= Files_db.remove_folder ~user_email:user.email ~path
          >>= function
          | Ok () -> Dream.redirect request "/dashboard"
          | Error msg -> Pages.error ~msg request |> serve msg)
      | _ -> Dream.empty `Bad_Request
  end
end

module Promises = struct
  let respond x : Dream.response Lwt.t =
    x |> tyxml_list_to_string
    |> Dream.respond ~status:`OK ~code:200
         ~headers:[ ("Content-Type", Dream.text_html) ]

  let static mime x : Dream.response Lwt.t =
    x |> Dream.respond ~status:`OK ~code:200 ~headers:mime

  let dashboard_files request =
    Files_db.fetch Files_db.branch >>= Files_db.all_files >>= fun files ->
    Pages.Promises.dashboard_files files request |> respond

  let config_details request =
    let before = Dream.param request "path" in
    let[@alert "-deprecated"] path = Dream.path request in
    let path =
      if List.hd path = "dashboard" then [ before ] else before :: path
    in
    Files_db.fetch Files_db.branch >>= fun np ->
    let* folder = Files_db.folder path np in
    Files_db.config_file path np >>= fun config ->
    Pages.Promises.config_details config folder
    |> Session.with_user request |> respond

  let edit_config request =
    let[@alert "-deprecated"] path = Dream.path request in
    Files_db.fetch Files_db.branch >>= fun np ->
    let* folder = Files_db.folder path np in
    Files_db.config_file path np >>= fun config ->
    Files_db.scripts scripts np >>= fun scripts ->
    Pages.Promises.config_edit_content folder scripts config request |> respond

  let edit_default_config request =
    let folder =
      Files.Folder.(Folder { name = ""; path = "." |> Fpath.v; files = [] })
    in
    Files_db.fetch Files_db.branch >>= fun np ->
    Files_db.config_file [ "config" ] np >>= fun config ->
    Files_db.scripts scripts np >>= fun scripts ->
    Pages.Promises.config_edit_content folder scripts config request |> respond

  let file_details request =
    let[@alert "-deprecated"] path = Dream.path request in
    let name, in_where, out_where, path =
      if List.hd path = "dashboard" then
        let name = Dream.param request "path" in
        (name, [ inputs ], [ outputs ], name)
      else
        let before = Dream.param request "path" in
        let path = before :: path in
        let name = Files.last_element path |> Option.value ~default:"" in
        let path = path |> List.rev |> List.tl |> List.rev in
        ( name,
          [ inputs ] @ path,
          [ outputs ] @ path,
          path @ [ name ] |> String.concat "/" )
    in
    Files_db.fetch Files_db.branch
    >>= Files_db.get_file ~in_where ~out_where ~name
    >>= fun f -> Pages.Promises.file_content path f request |> respond

  let output request =
    let[@alert "-deprecated"] path = Dream.path request in
    let name, where =
      if List.hd path = "dashboard" then
        let name = Dream.param request "path" in
        (name, [ outputs ])
      else
        let before = Dream.param request "path" in
        let path = before :: path in
        let name = Files.last_element path |> Option.value ~default:"" in
        let path = path |> List.rev |> List.tl |> List.rev in
        (name, [ outputs ] @ path)
    in
    Files_db.fetch Files_db.branch
    >>= Files_db.get_output ~where ~name
    >>= static (Dream.mime_lookup name)
end