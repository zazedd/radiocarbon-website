open Lwt.Infix
open Lwt.Syntax
open Website_lib
open Util

module Get = struct
  let file resource_path request =
    let[@alert "-deprecated"] path_request = Dream.path request in
    let resource_path = get_resource_path resource_path path_request in
    let extra_scripts =
      [
        Pages.Scripts.loader ~extra_action:"dropdowns (); confirm ()"
          resource_path;
        Pages.Scripts.dropdown;
        Pages.Scripts.confirm;
      ]
    in
    Pages.file request |> serve ~extra_scripts "Radiocarbon Calibration"

  let edit resource_path request =
    let[@alert "-deprecated"] path_request = Dream.path request in
    let resource_path = get_resource_path resource_path path_request in
    let extra_scripts = [ Pages.Scripts.loader resource_path ] in
    Pages.file request |> serve ~extra_scripts "Radiocarbon Calibration"

  let output resource_path request =
    let[@alert "-deprecated"] path_request = Dream.path request in
    let resource_path = get_resource_path resource_path path_request in
    let extra_scripts =
      [
        Pages.Scripts.loader ~extra_action:"dropdowns ()" resource_path;
        Pages.Scripts.dropdown;
      ]
    in
    Pages.output request |> serve ~extra_scripts "Radiocarbon Calibration"

  let add request =
    let[@alert "-deprecated"] folder_path =
      let path = Dream.path request in
      if List.hd path = "dashboard" then "" else path |> String.concat "/"
    in
    Pages.add_file folder_path request |> serve "Radiocarbon Calibration"

  let add_folder request =
    let[@alert "-deprecated"] folder_path =
      let path = Dream.path request in
      Format.printf "GET %s@." (String.concat "/" path);
      if List.hd path = "dashboard" then "" else path |> String.concat "/"
    in
    Pages.add_folder folder_path request |> serve "Radiocarbon Calibration"

  let rename_folder request =
    let[@alert "-deprecated"] folder_path = Dream.path request in
    let old_name = folder_path |> List.rev |> List.hd in
    let path = folder_path |> String.concat "/" in
    Pages.rename_folder old_name path request |> serve "Radiocarbon Calibration"
end

module Post = struct
  let add request =
    let[@alert "-deprecated"] path = Dream.path request in
    let path =
      if List.hd path = "dashboard" then [ inputs ] else inputs :: path
    in
    match%lwt Dream.multipart request with
    | `Ok [ ("file", [ (name, content) ]) ] -> (
        match name with
        | Some name -> begin
            let fname = Fpath.v name in
            Session.grab_session request |> Result.get_ok |> fun user ->
            Files_db.fetch Files_db.branch >>= fun np ->
            if Fpath.has_ext ".csv" fname then
              let content = Files.Csv.sanitize content in
              Files_db.add_file ~user_email:user.email ~path ~name ~content np
              >>= function
              | Ok () -> Dream.redirect request "/dashboard"
              | Error msg -> Pages.error ~msg request |> serve msg
            else if Fpath.has_ext ".xlsx" fname then
              (* lets convert the various sheets into csvs and store them all *)
              let* csvs = Xlsx2csv.convert_content ~into:`Strings content in
              match csvs with
              | Ok (`Strings contents) -> begin
                  (* lets first check if every sheet was converted correctly. if not we return early *)
                  let l =
                    List.filter_map
                      (function Ok _ -> None | Error (`Msg msg) -> Some msg)
                      contents
                  in
                  if List.length l <> 0 then
                    let msg = List.hd l in
                    Pages.error ~msg request |> serve msg
                  else
                    Files_db.add_files ~user_email:user.email ~path ~name
                      ~contents np
                    >>= function
                    | Ok _ -> Dream.redirect request "/dashboard"
                    | Error msg -> Pages.error ~msg request |> serve msg
                end
              | Error (`Msg msg) -> Pages.error ~msg request |> serve msg
              | _ -> assert false
            else
              let msg = "File should be a CSV or an Excel file!" in
              Pages.error ~msg request |> serve msg
          end
        | None ->
            let msg = "File has no name" in
            Pages.error ~msg request |> serve msg)
    | _ -> Dream.empty `Bad_Request

  let edit request =
    let[@alert "-deprecated"] path = Dream.path request in
    let path =
      let name = Files.last_element path |> Option.value ~default:"" in
      if List.hd path = "dashboard" then [ inputs; name ] else inputs :: path
    in
    match%lwt Dream.form request with
    | `Ok [ ("content", content) ] -> (
        Session.grab_session request |> Result.get_ok |> fun user ->
        Files_db.fetch Files_db.branch
        >>= Files_db.edit_file ~user_email:user.email ~path content
        >>= function
        | Ok () -> Dream.redirect request "/dashboard"
        | Error msg -> Pages.error ~msg request |> serve msg)
    | _ -> Dream.empty `Bad_Request

  let remove request =
    let[@alert "-deprecated"] path = Dream.path request in
    let name, in_where, out_where =
      let name = Files.last_element path |> Option.value ~default:"" in
      let path = path |> List.rev |> List.tl |> List.rev in
      (name, inputs :: path, outputs :: path)
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
    let[@alert "-deprecated"] path = Dream.path request in
    let in_where, out_where =
      if List.hd path = "dashboard" then ([ inputs ], [ outputs ])
      else (inputs :: path, outputs :: path)
    in
    match%lwt Dream.form request with
    | `Ok [ ("name", name) ] -> (
        Session.grab_session request |> Result.get_ok |> fun user ->
        Files_db.fetch Files_db.branch
        >>= Files_db.add_folder ~user_email:user.email ~in_where ~out_where
              ~name
        >>= function
        | Ok () -> Dream.redirect request "/dashboard"
        | Error msg -> Pages.error ~msg request |> serve msg)
    | _ -> Dream.empty `Bad_Request

  let rename_folder request =
    let[@alert "-deprecated"] path = Dream.path request in
    if List.hd path = "dashboard" then
      let msg = "Do not rename the inputs folder" in
      Pages.error ~msg request |> serve msg
    else
      let old_name, in_where, out_where =
        let name = Files.last_element path |> Option.value ~default:"" in
        let path = path |> List.rev |> List.tl |> List.rev in
        (name, inputs :: path, outputs :: path)
      in
      match%lwt Dream.form request with
      | `Ok [ ("name", new_name) ] -> (
          Session.grab_session request |> Result.get_ok |> fun user ->
          Files_db.fetch Files_db.branch
          >>= Files_db.rename_folder ~user_email:user.email ~in_where ~out_where
                ~old_name ~new_name
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

module Promises = struct
  let details request =
    let[@alert "-deprecated"] path = Dream.path request in
    let name, in_where, out_where, path = resolve_path_details path request in
    Files_db.fetch Files_db.branch >>= fun np ->
    Files_db.get_file ~in_where ~out_where ~name np >>= fun f ->
    let configs = Hashtbl.create 0 in
    Hashtbl.fold
      (fun hash _ acc ->
        let promise =
          let+ x =
            Files_db.historic_config_from_commit_hash ~hash ~in_where np
          in
          match x with
          | Ok c -> Ok (Hashtbl.add configs hash c)
          | Error _ as err -> err
        in
        promise :: acc)
      f.outputs []
    |> Lwt.all
    >>= fun results ->
    results |> List.filter (function Ok _ -> false | Error _ -> true)
    |> function
    | [] -> Pages.Promises.file_content path f configs request |> respond
    | l -> List.hd l |> Pages.error |> respond

  let edit request =
    let[@alert "-deprecated"] path = Dream.path request in
    let name, in_where, out_where, path = resolve_path_details path request in
    Files_db.fetch Files_db.branch >>= fun np ->
    Files_db.get_file ~in_where ~out_where ~name np >>= fun f ->
    Pages.Promises.edit_file_content path f request |> respond

  let output request =
    let[@alert "-deprecated"] path = Dream.path request in
    let name, in_where, out_where, path = resolve_path_details path request in
    Files_db.fetch Files_db.branch
    >>= Files_db.historic_input_config_and_script ~in_where ~out_where ~name
    >>= function
    | Ok (input_content, config, script_content) ->
        Pages.Promises.output_file_content name path input_content config
          script_content request
        |> respond
    | Error msg -> Pages.error msg |> respond

  let output_file request =
    let[@alert "-deprecated"] path = Dream.path request in
    let name, _, out_where, _ = resolve_path_details path request in
    Files_db.fetch Files_db.branch
    >>= Files_db.get_output ~where:out_where ~name
    >>= static (Dream.mime_lookup name)
end
