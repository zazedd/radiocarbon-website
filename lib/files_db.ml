open Lwt.Infix
open Lwt.Syntax
module Store = Files.Store
module Tree = Store.Tree

let branch = Db_config.branch
let inputs = Db_config.inputs
let scripts_folder = Db_config.scripts
let config = Db_config.config

type config = {
  step : int;
  confidence : float;
  column : string option;
  value : string option;
  script : string;
  typ : [ `Custom | `Default ];
}

let typ_to_str = function `Custom -> "custom" | `Default -> "default"
let opt_to_str = function Some s -> s | None -> "none"

let print_config c =
  Format.printf
    "step=%d\nconfidence=%.2f\ncolumn=%s\nvalue=%s\nscript=%s\ntyp=%s@." c.step
    c.confidence (c.column |> opt_to_str) (c.value |> opt_to_str) c.script
    (c.typ |> typ_to_str)

let parse_config s typ =
  let extract_value s pref f =
    match String.split_on_char '=' s with
    | [ p; "" ] when p = pref -> None
    | [ p; v ] when p = pref -> Some (f v)
    | _ -> assert false
  in
  match s |> String.trim |> String.split_on_char '\n' with
  | [ step; confidence; column; value; script ] ->
      {
        step = extract_value step "step" int_of_string |> Option.get;
        confidence =
          extract_value confidence "confidence" float_of_string |> Option.get;
        column = extract_value column "column" (fun x -> x);
        value = extract_value value "value" (fun x -> x);
        script = extract_value script "script" (fun x -> x) |> Option.get;
        typ;
      }
  | _ -> failwith "Failed to parse config"

let create_config step confidence column value script =
  let column, value =
    (Option.value ~default:"" column, Option.value ~default:"" value)
  in
  Format.sprintf "step=%d\nconfidence=%.02f\ncolumn=%s\nvalue=%s\nscript=%s"
    step confidence column value script

let fetch branch =
  Files.ctx () >>= fun ctx ->
  let* repo = Files.Store.Repo.v Db_config.Pipeline.c in
  Files.fetch ~ctx ~repo ~branch >|= fun node -> (node, repo)

let push branch =
  Files.ctx () >>= fun ctx ->
  let* repo = Files.Store.Repo.v Db_config.Pipeline.c in
  Files.push ~ctx ~repo ~branch

(* TODO: ALL OF THESE CAN BE CACHED IF IT DOESNT FETCH ANYTHING *)

let all_files (node, repo) =
  match node with
  | Ok _ ->
      let* tree =
        Files.Store.of_branch repo branch >>= fun t -> Files.Store.get_tree t []
      in
      Files.Folder.get_custom_tree ~order:`Sorted ~tree ~where:[ inputs ]
      >|= fun folder ->
      Pipeline_cache.set folder;
      folder
  | Error (`Msg msg) -> failwith msg

let folder folder_name (node, repo) : Files.Folder.t Lwt.t =
  match node with
  | Ok _ ->
      let* tree =
        Files.Store.of_branch repo branch >>= fun t -> Files.Store.get_tree t []
      in
      Files.Folder.get_custom_tree ~order:`Sorted ~tree
        ~where:(inputs :: folder_name)
  | Error (`Msg msg) -> failwith msg

let config_file folder_name (node, repo) : config Lwt.t =
  match node with
  | Ok _ ->
      Files.Store.of_branch repo branch >>= fun t ->
      let* b = Files.Store.mem t ([ inputs ] @ folder_name @ [ config ]) in
      if b then begin
        Files.Store.get t ([ inputs ] @ folder_name @ [ config ])
        >|= fun content -> parse_config content `Custom
      end
      else begin
        Files.Store.get t [ config ] >|= fun content ->
        parse_config content `Default
      end
  | Error (`Msg msg) -> failwith msg

let scripts script_folder (node, repo) : string list Lwt.t =
  match node with
  | Ok _ ->
      let* tree =
        Files.Store.of_branch repo branch >>= fun t -> Files.Store.get_tree t []
      in
      Files.Folder.file_names ~order:`Undefined ~tree ~where:[ script_folder ]
  | Error (`Msg msg) -> failwith msg

let get_file ~in_where ~out_where ~name (node, repo) =
  match node with
  | Ok _ ->
      let* tree =
        Files.Store.of_branch repo branch >>= fun t -> Files.Store.get_tree t []
      in
      Files.File.get_file ~tree ~in_where ~out_where ~name
  | Error (`Msg msg) -> failwith msg

let get_output ~where ~name (node, repo) =
  match node with
  | Ok _ ->
      let* tree =
        Files.Store.of_branch repo branch >>= fun t ->
        Files.Store.get_tree t where
      in
      Files.Store.Tree.get tree [ name ]
  | Error (`Msg msg) -> failwith msg

let add_config ~user_email ~path ~step ~confidence ~column ~value ~script
    (node, repo) =
  let config_path = path @ [ "config" ] in
  match node with
  | Ok _ ->
      let* t = Files.Store.of_branch repo branch in
      let config = create_config step confidence column value script in
      let* c = Files.Store.kind t path in
      if c = Some `Node then
        let* () =
          Files.Store.set_exn t
            ~info:(Db_config.Pipeline.info "Adding config" user_email)
            config_path config
        in
        push branch
        >|= begin
              function
              | Ok () -> Ok ()
              | Error (`Msg msg) -> Error msg
              | Error `Detached_head -> Error "Detached head"
            end
      else Error "Parent directory does not exist" |> Lwt.return
  | Error (`Msg msg) -> Error msg |> Lwt.return

let update_config ~user_email ~path ~step ~confidence ~column ~value ~script
    (node, repo) =
  match node with
  | Ok _ ->
      let* t = Files.Store.of_branch repo branch in
      let config = create_config step confidence column value script in
      let* c = Files.Store.mem t path in
      if c then
        let* () =
          Files.Store.set_exn t
            ~info:(Db_config.Pipeline.info "Updating config" user_email)
            path config
        in
        push branch
        >|= begin
              function
              | Ok () -> Ok ()
              | Error (`Msg msg) -> Error msg
              | Error `Detached_head -> Error "Detached head"
            end
      else Error "Config does not exist" |> Lwt.return
  | Error (`Msg msg) -> Error msg |> Lwt.return

let remove_config ~user_email ~path (node, repo) =
  match node with
  | Ok _ ->
      let* t = Files.Store.of_branch repo branch in
      let* c = Files.Store.mem t path in
      if c then
        let* () =
          Files.Store.remove_exn t
            ~info:(Db_config.Pipeline.info "Removing config" user_email)
            path
        in
        push branch
        >|= begin
              function
              | Ok () -> Ok ()
              | Error (`Msg msg) -> Error msg
              | Error `Detached_head -> Error "Detached head"
            end
      else Error "Config does not exist" |> Lwt.return
  | Error (`Msg msg) -> Error msg |> Lwt.return

let add_folder ~user_email ~path ~name (node, repo) =
  let path = List.filter (( <> ) "") path in
  let new_file_path = path @ [ name; ".ign" ] in
  match node with
  | Ok _ ->
      let* t = Files.Store.of_branch repo branch in
      let* c = Files.Store.kind t path in
      if c = Some `Node then
        let* () =
          Files.Store.set_exn t
            ~info:(Db_config.Pipeline.info "Adding folder" user_email)
            new_file_path ""
        in
        push branch
        >|= begin
              function
              | Ok () -> Ok ()
              | Error (`Msg msg) -> Error msg
              | Error `Detached_head -> Error "Detached head"
            end
      else Error "Parent directory does not exist" |> Lwt.return
  | Error (`Msg msg) -> Error msg |> Lwt.return

let remove_folder ~user_email ~path (node, repo) =
  let path = List.filter (( <> ) "") path in
  match node with
  | Ok _ ->
      let* t = Files.Store.of_branch repo branch in
      let* c = Files.Store.kind t path in
      if c = Some `Node then
        let* () =
          Files.Store.remove_exn t
            ~info:(Db_config.Pipeline.info "Removing folder" user_email)
            path
        in
        push branch
        >|= begin
              function
              | Ok () -> Ok ()
              | Error (`Msg msg) -> Error msg
              | Error `Detached_head -> Error "Detached head"
            end
      else Error "Parent directory does not exist" |> Lwt.return
  | Error (`Msg msg) -> Error msg |> Lwt.return

let rename_folder ~user_email ~path ~old_name ~new_name (node, repo) =
  let path = List.filter (( <> ) "") path in
  let old_folder_path = path @ [ old_name ] in
  let new_folder_path = path @ [ new_name ] in
  match node with
  | Ok _ ->
      let* t = Files.Store.of_branch repo branch in
      let* c = Files.Store.kind t old_folder_path in
      let* root_tree = Files.Store.tree t in
      if c = Some `Node then
        let* file_tree = Files.Store.Tree.get_tree root_tree old_folder_path in
        Files.Store.Tree.update_tree root_tree new_folder_path (fun _ ->
            Some file_tree)
        >>= fun tree ->
        Files.Store.Tree.remove tree old_folder_path >>= fun tree ->
        let* () =
          Files.Store.set_tree_exn
            ~info:(Db_config.Pipeline.info "Renaming folder" user_email)
            t [] tree
        in
        push branch
        >|= begin
              function
              | Ok () -> Ok ()
              | Error (`Msg msg) -> Error msg
              | Error `Detached_head -> Error "Detached head"
            end
      else Error "Directory does not exist" |> Lwt.return
  | Error (`Msg msg) -> Error msg |> Lwt.return

let add_file ~user_email ~path ~name ~content (node, repo) =
  let path = List.filter (( <> ) "") path in
  let new_file_path = path @ [ name ] in
  match node with
  | Ok _ ->
      let* t = Files.Store.of_branch repo branch in
      let* c = Files.Store.kind t path in
      if c = Some `Node then
        let* () =
          Files.Store.set_exn t
            ~info:(Db_config.Pipeline.info "Adding file" user_email)
            new_file_path content
        in
        push branch
        >|= begin
              function
              | Ok () -> Ok ()
              | Error (`Msg msg) -> Error msg
              | Error `Detached_head -> Error "Detached head"
            end
      else Error "Parent directory does not exist" |> Lwt.return
  | Error (`Msg msg) -> Error msg |> Lwt.return

let remove_file ~user_email ~in_where ~out_where ~name (node, repo) =
  let outputs = List.hd out_where in
  let in_path = in_where @ [ name ] in
  match node with
  | Ok _ ->
      let* t = Files.Store.of_branch repo branch in
      let* c = Files.Store.kind t in_where in
      let* root_tree = Files.Store.get_tree t [] in
      if c = Some `Node then
        let* file =
          Files.File.get_file ~tree:root_tree ~in_where ~out_where ~name
        in
        Files.Store.Tree.remove root_tree in_path >>= fun tree ->
        Lwt_list.fold_left_s
          (fun tree (_, _, output_path) ->
            Format.printf "%s@." (output_path |> Fpath.to_string);
            let output_path =
              outputs
              :: (output_path |> Fpath.to_string |> String.split_on_char '/')
            in
            Files.Store.Tree.remove tree output_path)
          tree file.outputs
        >>= Files.Store.set_tree_exn
              ~info:(Db_config.Pipeline.info "Removing file" user_email)
              t []
        >>= fun () ->
        push branch
        >|= begin
              function
              | Ok () -> Ok ()
              | Error (`Msg msg) -> Error msg
              | Error `Detached_head -> Error "Detached head"
            end
      else Error "Parent directory does not exist" |> Lwt.return
  | Error (`Msg msg) -> Error msg |> Lwt.return

let find_earliest_commit_with_file ~path (node, repo) =
  match node with
  | Ok _ -> begin
      Files.Store.of_branch repo branch >>= fun t ->
      Store.last_modified t path >|= fun lst -> Ok (List.hd lst)
    end
  | Error (`Msg msg) -> Error msg |> Lwt.return

let historic_input_config_and_script ~in_where ~out_where ~name np =
  let input_file =
    let name =
      (* name.script.date.csv *)
      name |> Fpath.v |> Fpath.rem_ext ~multi:true
      (* name *) |> Fpath.add_ext ".csv" (* name.csv *)
      |> Fpath.to_string
    in
    in_where @ [ name ]
  in
  let script_file =
    (* n.script.date.csv or n.script.type.date.pdf *)
    let name_fpath = name |> Fpath.v in
    let tmp =
      name_fpath |> Fpath.rem_ext
      |> Fpath.rem_ext (* n.script or n.script.type *)
    in
    let name =
      (if name_fpath |> Fpath.get_ext = ".csv" then tmp
       else tmp |> Fpath.rem_ext)
      (* n.script *) |> Fpath.get_ext (* .script *)
      |> Files.remove_dot |> Fpath.v (* script *)
      |> Fpath.add_ext ".r" (* script.r *)
      |> Fpath.to_string
    in
    [ scripts_folder; name ]
  in
  let output_file = out_where @ [ name ] in
  let config_file = in_where @ [ config ] in
  find_earliest_commit_with_file ~path:output_file np >>= function
  | Ok commit -> begin
      Files.Store.of_commit commit >>= fun t ->
      let+ config =
        let* b = Files.Store.mem t config_file in
        if b then
          Files.Store.get t config_file >|= fun str -> parse_config str `Custom
        else
          Files.Store.get t [ config ] >|= fun str -> parse_config str `Default
      and+ input_content = Files.Store.get t input_file
      and+ script_content = Files.Store.get t script_file in
      Ok (input_content, config, script_content)
    end
  | Error _ as err -> err |> Lwt.return
