open Lwt.Infix

let tyxml_list_to_string html =
  List.map (Format.asprintf "%a" (Tyxml.Html.pp_elt ())) html
  |> String.concat ""

let pipeline_status_topbar_path = "/dashboard/get-pipeline-status/topbar"
let pipeline_status_popup_path = "/dashboard/get-pipeline-status/popup"
let inputs = Website_lib.Db_config.inputs
let outputs = Website_lib.Db_config.outputs
let scripts = Website_lib.Db_config.scripts

let resolve_path_details path _request =
  if List.hd path = "dashboard" then
    let name = path |> List.tl |> List.hd in
    (name, [ inputs ], [ outputs ], name)
  else
    let name =
      Website_lib.Files.last_element path |> Option.value ~default:""
    in
    let path = path |> List.rev |> List.tl |> List.rev in
    (name, inputs :: path, outputs :: path, path @ [ name ] |> String.concat "/")

let add_contribution_query id path =
  let query = [ ("contribution", [ id ]) ] in
  Uri.make ~path ~query () |> Uri.to_string

let branch_and_contrib_from_query request =
  match Dream.query request "contribution" with
  | Some id ->
      Website_lib.Contributions_db.get id >|= fun contrib ->
      ("contrib_" ^ id, Some contrib)
  | None -> (Website_lib.Files_db.branch, None) |> Lwt.return

let redirect_from_query request path =
  match Dream.query request "contribution" with
  | Some id ->
      let uri = Uri.make ~path () in
      Uri.add_query_param uri ("contribution", [ id ]) |> Uri.to_string
  | None -> path

let redirect_from_contrib contrib path =
  match contrib with
  | Some contrib ->
      let uri = Uri.make ~path () in
      Uri.add_query_param uri
        ( "contribution",
          [ Website_lib.Contributions.(contrib.id) |> string_of_int ] )
      |> Uri.to_string
  | None -> path

let get_resource_path resource_path path_request =
  if List.hd path_request = "dashboard" then
    resource_path ^ (List.tl path_request |> String.concat "/")
  else resource_path ^ "/" ^ (path_request |> String.concat "/")

let serve ?contrib ?(extra_scripts = [ Tyxml.Html.(script (txt "")) ]) title
    page =
  let topbar_path =
    pipeline_status_topbar_path |> redirect_from_contrib contrib
  in
  let popup_path =
    pipeline_status_popup_path |> redirect_from_contrib contrib
  in
  let status_scripts =
    [
      Pages.Scripts.pipeline_topbar_popup_loader ~topbar_path ~popup_path;
      Pages.Scripts.pipeline_popup;
    ]
  in
  Dream.html
  @@ (Pages.base title page (status_scripts @ extra_scripts)
     |> tyxml_list_to_string)

let respond x : Dream.response Lwt.t =
  x |> tyxml_list_to_string
  |> Dream.respond ~status:`OK ~code:200
       ~headers:[ ("Content-Type", Dream.text_html) ]

let static headers x : Dream.response Lwt.t =
  x |> Dream.respond ~status:`OK ~code:200 ~headers
