open Lwt.Infix
open Lwt.Syntax
open Website_lib
open Util

module Get = struct
  let file resource_path request =
    let[@alert "-deprecated"] path_request = Dream.path request in
    let* _, contrib = branch_and_contrib_from_query request in
    match contrib with
    | (Some (Ok _) | None) as contrib -> begin
        let resource_path =
          resource_path_with_query resource_path path_request contrib
        in
        let extra_scripts =
          [
            Pages.Scripts.loader ~extra_action:"dropdowns (); confirm ()"
              resource_path;
            Pages.Scripts.dropdown;
            Pages.Scripts.confirm;
          ]
        in
        Pages.file request
        |> serve ?contrib ~extra_scripts "Radiocarbon Calibration"
      end
    | Some (Error msg) -> Pages.error ~msg request |> respond

  let edit resource_path request =
    let[@alert "-deprecated"] path_request = Dream.path request in
    let* _, contrib = branch_and_contrib_from_query request in
    match contrib with
    | (Some (Ok _) | None) as contrib ->
        let resource_path =
          resource_path_with_query resource_path path_request contrib
        in
        let extra_scripts = [ Pages.Scripts.loader resource_path ] in
        Pages.file request
        |> serve ?contrib ~extra_scripts "Radiocarbon Calibration"
    | Some (Error msg) -> Pages.error ~msg request |> respond

  let output resource_path request =
    let[@alert "-deprecated"] path_request = Dream.path request in
    let* _, contrib = branch_and_contrib_from_query request in
    match contrib with
    | (Some (Ok _) | None) as contrib ->
        let resource_path =
          resource_path_with_query resource_path path_request contrib
        in
        let extra_scripts =
          [
            Pages.Scripts.loader ~extra_action:"dropdowns ()" resource_path;
            Pages.Scripts.dropdown;
          ]
        in
        Pages.output request
        |> serve ?contrib ~extra_scripts "Radiocarbon Calibration"
    | Some (Error msg) -> Pages.error ~msg request |> respond

  let add request =
    let[@alert "-deprecated"] folder_path =
      let path = Dream.path request in
      if List.hd path = "dashboard" then "" else path |> String.concat "/"
    in
    let* _, contrib = branch_and_contrib_from_query request in
    match contrib with
    | Some (Ok contribution) ->
        Pages.add_file
          ~contrib_id:(contribution.id |> string_of_int)
          folder_path request
        |> serve ?contrib "Radiocarbon Calibration"
    | None ->
        Pages.add_file folder_path request |> serve "Radiocarbon Calibration"
    | Some (Error msg) -> Pages.error ~msg request |> respond

  let add_folder request =
    let[@alert "-deprecated"] folder_path =
      let path = Dream.path request in
      Format.printf "GET %s@." (String.concat "/" path);
      if List.hd path = "dashboard" then "" else path |> String.concat "/"
    in
    let* _, contrib = branch_and_contrib_from_query request in
    match contrib with
    | Some (Ok contribution) ->
        Pages.add_folder
          ~contrib_id:(contribution.id |> string_of_int)
          folder_path request
        |> serve "Radiocarbon Calibration"
    | None ->
        Pages.add_folder folder_path request |> serve "Radiocarbon Calibration"
    | Some (Error msg) -> Pages.error ~msg request |> respond

  let rename_folder request =
    let[@alert "-deprecated"] folder_path = Dream.path request in
    let old_name = folder_path |> List.rev |> List.hd in
    let path = folder_path |> String.concat "/" in
    let* _, contrib = branch_and_contrib_from_query request in
    match contrib with
    | Some (Ok contribution) ->
        Pages.rename_folder
          ~contrib_id:(contribution.id |> string_of_int)
          old_name path request
        |> serve "Radiocarbon Calibration"
    | None ->
        Pages.rename_folder old_name path request
        |> serve "Radiocarbon Calibration"
    | Some (Error msg) -> Pages.error ~msg request |> respond
end

module Post = struct
  let add request =
    let[@alert "-deprecated"] path = Dream.path request in
    let path =
      if List.hd path = "dashboard" then [ inputs ] else inputs :: path
    in
    let* branch, contrib = branch_and_contrib_from_query request in
    match contrib with
    | Some (Ok _) | None -> begin
        match%lwt Dream.multipart request with
        | `Ok [ ("file", [ (name, content) ]) ] -> (
            match name with
            | Some name -> begin
                let fname = Fpath.v name in
                Session.grab_session request |> Result.get_ok |> fun user ->
                Files_db.fetch branch >>= fun np ->
                if Fpath.has_ext ".csv" fname then
                  let content = Files.Csv.sanitize content in
                  Files_db.add_file ~branch ~user_email:user.email ~path ~name
                    ~content np
                  >>= function
                  | Ok () ->
                      Dream.redirect request
                        ("/dashboard" |> redirect_from_query request)
                  | Error msg -> Pages.error ~msg request |> serve msg
                else if Fpath.has_ext ".xlsx" fname then
                  (* lets convert the various sheets into csvs and store them all *)
                  let* csvs = Xlsx2csv.convert_content ~into:`Strings content in
                  match csvs with
                  | Ok (`Strings contents) -> begin
                      (* lets first check if every sheet was converted correctly. if not we return early *)
                      let l =
                        List.filter_map
                          (function
                            | Ok _ -> None | Error (`Msg msg) -> Some msg)
                          contents
                      in
                      if List.length l <> 0 then
                        let msg = List.hd l in
                        Pages.error ~msg request |> serve msg
                      else
                        Files_db.add_files ~branch ~user_email:user.email ~path
                          ~name ~contents np
                        >>= function
                        | Ok _ ->
                            Dream.redirect request
                              ("/dashboard" |> redirect_from_query request)
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
      end
    | Some (Error msg) -> Pages.error ~msg request |> respond

  let edit request =
    let[@alert "-deprecated"] path = Dream.path request in
    let path =
      let name = Files.last_element path |> Option.value ~default:"" in
      if List.hd path = "dashboard" then [ inputs; name ] else inputs :: path
    in
    let* branch, contrib = branch_and_contrib_from_query request in
    match contrib with
    | Some (Ok _) | None -> begin
        match%lwt Dream.form request with
        | `Ok [ ("content", content) ] -> (
            Session.grab_session request |> Result.get_ok |> fun user ->
            Files_db.fetch branch
            >>= Files_db.edit_file ~branch ~user_email:user.email ~path content
            >>= function
            | Ok () ->
                Dream.redirect request
                  ("/dashboard" |> redirect_from_query request)
            | Error msg -> Pages.error ~msg request |> serve msg)
        | _ -> Dream.empty `Bad_Request
      end
    | Some (Error msg) -> Pages.error ~msg request |> respond

  let remove request =
    let[@alert "-deprecated"] path = Dream.path request in
    let name, in_where, out_where =
      let name = Files.last_element path |> Option.value ~default:"" in
      let path = path |> List.rev |> List.tl |> List.rev in
      (name, inputs :: path, outputs :: path)
    in
    let* branch, contrib = branch_and_contrib_from_query request in
    match contrib with
    | Some (Ok _) | None -> begin
        match%lwt Dream.form request with
        | `Ok [ ("hidden", "hidden") ] -> (
            Session.grab_session request |> Result.get_ok |> fun user ->
            Files_db.fetch branch
            >>= Files_db.remove_file ~branch ~user_email:user.email ~in_where
                  ~out_where ~name
            >>= function
            | Ok () ->
                Dream.redirect request
                  ("/dashboard" |> redirect_from_query request)
            | Error msg -> Pages.error ~msg request |> serve msg)
        | _ -> Dream.empty `Bad_Request
      end
    | Some (Error msg) -> Pages.error ~msg request |> respond

  let add_folder request =
    let[@alert "-deprecated"] path = Dream.path request in
    let in_where, out_where =
      if List.hd path = "dashboard" then ([ inputs ], [ outputs ])
      else (inputs :: path, outputs :: path)
    in
    let* branch, contrib = branch_and_contrib_from_query request in
    match contrib with
    | Some (Ok _) | None -> begin
        match%lwt Dream.form request with
        | `Ok [ ("name", name) ] -> (
            Session.grab_session request |> Result.get_ok |> fun user ->
            Files_db.fetch branch
            >>= Files_db.add_folder ~branch ~user_email:user.email ~in_where
                  ~out_where ~name
            >>= function
            | Ok () ->
                Dream.redirect request
                  ("/dashboard" |> redirect_from_query request)
            | Error msg -> Pages.error ~msg request |> serve msg)
        | _ -> Dream.empty `Bad_Request
      end
    | Some (Error msg) -> Pages.error ~msg request |> respond

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
      let* branch, contrib = branch_and_contrib_from_query request in
      match contrib with
      | Some (Ok _) | None -> begin
          match%lwt Dream.form request with
          | `Ok [ ("name", new_name) ] -> (
              Session.grab_session request |> Result.get_ok |> fun user ->
              Files_db.fetch branch
              >>= Files_db.rename_folder ~branch ~user_email:user.email
                    ~in_where ~out_where ~old_name ~new_name
              >>= function
              | Ok () ->
                  Dream.redirect request
                    ("/dashboard" |> redirect_from_query request)
              | Error msg -> Pages.error ~msg request |> serve msg)
          | _ -> Dream.empty `Bad_Request
        end
      | Some (Error msg) -> Pages.error ~msg request |> respond

  let remove_folder request =
    let[@alert "-deprecated"] path = inputs :: Dream.path request in
    let* branch, contrib = branch_and_contrib_from_query request in
    match contrib with
    | Some (Ok _) | None -> begin
        match%lwt Dream.form request with
        | `Ok [ ("hidden", "hidden") ] -> (
            Session.grab_session request |> Result.get_ok |> fun user ->
            Files_db.fetch branch
            >>= Files_db.remove_folder ~branch ~user_email:user.email ~path
            >>= function
            | Ok () ->
                Dream.redirect request
                  ("/dashboard" |> redirect_from_query request)
            | Error msg -> Pages.error ~msg request |> serve msg)
        | _ -> Dream.empty `Bad_Request
      end
    | Some (Error msg) -> Pages.error ~msg request |> respond
end

module Promises = struct
  let details request =
    let[@alert "-deprecated"] path = Dream.path request in
    let* branch, contrib = branch_and_contrib_from_query request in
    let name, in_where, out_where, path = resolve_path_details path request in
    Files_db.fetch branch >>= fun np ->
    Files_db.get_file ~branch ~in_where ~out_where ~name np >>= fun f ->
    match contrib with
    | (Some (Ok _) | None) as c -> begin
        let contrib_id =
          match c with
          | Some (Ok contrib) -> Some (contrib.id |> string_of_int)
          | None -> None
          | Some (Error _) -> assert false
        in
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
        | [] ->
            Pages.Promises.file_content ~contrib_id path f configs request
            |> respond
        | l -> List.hd l |> Pages.error |> respond
      end
    | Some (Error msg) -> Pages.error ~msg request |> respond

  let edit request =
    let[@alert "-deprecated"] path = Dream.path request in
    let name, in_where, out_where, path = resolve_path_details path request in
    let* branch, contrib = branch_and_contrib_from_query request in
    match contrib with
    | Some (Ok contribution) -> begin
        Files_db.fetch branch >>= fun np ->
        Files_db.get_file ~branch ~in_where ~out_where ~name np >>= fun f ->
        Pages.Promises.edit_file_content
          ~contrib_id:(contribution.id |> string_of_int)
          path f request
        |> respond
      end
    | None -> begin
        Files_db.fetch branch >>= fun np ->
        Files_db.get_file ~branch ~in_where ~out_where ~name np >>= fun f ->
        Pages.Promises.edit_file_content path f request |> respond
      end
    | Some (Error msg) -> Pages.error ~msg request |> respond

  let output request =
    let[@alert "-deprecated"] path = Dream.path request in
    let name, in_where, out_where, path = resolve_path_details path request in
    let* branch, contrib = branch_and_contrib_from_query request in
    match contrib with
    | Some (Ok _) | None -> begin
        Files_db.fetch branch
        >>= Files_db.historic_input_config_and_script ~branch ~in_where
              ~out_where ~name
        >>= function
        | Ok (input_content, config, script_content) ->
            Pages.Promises.output_file_content name path input_content config
              script_content request
            |> respond
        | Error msg -> Pages.error msg |> respond
      end
    | Some (Error msg) -> Pages.error ~msg request |> respond

  let output_file request =
    let[@alert "-deprecated"] path = Dream.path request in
    let name, _, out_where, _ = resolve_path_details path request in
    let* branch, contrib = branch_and_contrib_from_query request in
    match contrib with
    | Some (Ok _) | None -> begin
        Files_db.fetch branch
        >>= Files_db.get_output ~branch ~where:out_where ~name
        >>= static (Dream.mime_lookup name)
      end
    | Some (Error msg) -> Pages.error ~msg request |> respond
end
