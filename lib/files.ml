open Lwt.Syntax
open Lwt.Infix

let addr =
  Result.get_ok
    (Smart_git.Endpoint.of_string
       "git@github.com:zazedd/inputs-outputs-R14C.git")

let key =
  let x = open_in "env/github_sk.pem" |> input_line |> String.trim in
  x |> Awa.Keys.of_string |> function
  | Ok s -> s
  | Error (`Msg msg) -> failwith msg

let ctx () =
  Git_unix.ctx (Happy_eyeballs_lwt.create ()) >|= fun ctx ->
  Mimic.add Git_mirage_ssh.git_mirage_ssh_key key ctx

let rec last_element lst =
  match lst with [] -> None | [ x ] -> Some x | _ :: tl -> last_element tl

module Store = Irmin_git_unix.FS.KV (Irmin.Contents.String)
module Tree = Store.Tree

module Csv : sig
  val parse_csv : string -> string list list
end = struct
  let parse_csv str =
    let lines = String.split_on_char '\n' str in
    let rec h c line =
      let line = String.trim line in
      if line = "" then []
      else begin
        match String.split_on_char c line with
        | [ "" ] -> []
        | [ _ ] | [] -> h ';' line
        | lst -> lst
      end
    in
    List.map (h ',') lines |> List.filter (( <> ) [])
end

module File : sig
  type t = {
    name : Fpath.t;
    content : string;
    outputs : (string * string * Fpath.t) list;
    pdfs : (string * string * Fpath.t) list;
  }

  val get_file :
    tree:Store.tree ->
    in_where:string list ->
    out_where:Store.path ->
    name:string ->
    t Lwt.t
end = struct
  (* outputs -> (script name, date, file path) list *)
  (* pdfs -> (type of pdf, date, file path) list *)
  type t = {
    name : Fpath.t;
    content : string;
    outputs : (string * string * Fpath.t) list;
    pdfs : (string * string * Fpath.t) list;
  }

  let compare_output (_, date1, _) (_, date2, _) = compare date1 date2

  let is_output f1 f2 =
    let input_ext = Fpath.get_ext f1 and output_ext = Fpath.get_ext f2 in
    if input_ext = output_ext then
      let input_name = Fpath.rem_ext ~multi:true f1 in
      let output_name = Fpath.rem_ext ~multi:true f2 in
      input_name = output_name
    else false

  let is_pdf_output f1 f2 =
    if Fpath.get_ext f2 = ".pdf" then
      let input_name = Fpath.rem_ext ~multi:true f1 in
      let output_name = Fpath.rem_ext ~multi:true f2 in
      input_name = output_name
    else false

  (* inputname.script.date.csv *)
  let script_name fpath out_file =
    let out_path = Fpath.(fpath // out_file) in
    let date_dot = out_file |> Fpath.rem_ext |> Fpath.get_ext in
    let script_dot =
      out_file |> Fpath.rem_ext |> Fpath.rem_ext |> Fpath.get_ext
    in
    let date = String.sub date_dot 1 (String.length date_dot - 1) in
    let script = String.sub script_dot 1 (String.length script_dot - 1) in
    (script, date, out_path)

  (* inputname.script.type.date.pdf *)
  let pdf_name fpath out_file =
    let out_path = Fpath.(fpath // out_file) in
    let date_dot = out_file |> Fpath.rem_ext |> Fpath.get_ext
    and typ_dot = out_file |> Fpath.rem_ext |> Fpath.rem_ext |> Fpath.get_ext in
    let date = String.sub date_dot 1 (String.length date_dot - 1)
    and typ = String.sub typ_dot 1 (String.length typ_dot - 1) in
    (typ, date, out_path)

  let collect_list p (acc : t) (path, t) =
    if path = "" then acc |> Lwt.return
    else
      let fpath = path |> Fpath.v in
      let name = fpath |> Fpath.base in
      let* k = Tree.kind t [] in
      match k with
      | Some k' ->
          (match k' with
          | `Node -> acc
          | `Contents -> begin
              if is_output acc.name name then begin
                { acc with outputs = (name |> script_name p) :: acc.outputs }
              end
              else if is_pdf_output acc.name name then begin
                { acc with pdfs = (name |> pdf_name p) :: acc.pdfs }
              end
              else acc
            end)
          |> Lwt.return
      | None -> failwith "no kind"

  let get_file ~tree ~in_where ~out_where ~name =
    let out_path =
      match List.tl out_where with
      | [] -> Fpath.v "."
      | lst -> lst |> String.concat "/" |> Fpath.v
    in
    let* content = Tree.get tree (in_where @ [ name ]) in
    let name = Fpath.v name in
    let* tree = Tree.get_tree tree out_where in
    Tree.list tree [] >>= fun l ->
    Lwt_list.fold_left_s (collect_list out_path)
      { name; content; outputs = []; pdfs = [] }
      l
    >|= fun file ->
    {
      file with
      outputs = List.sort compare_output file.outputs;
      pdfs = List.sort compare_output file.pdfs;
    }
end

module Folder : sig
  type t =
    | File of { name : string; path : Fpath.t; content : string }
    | Folder of { name : string; path : Fpath.t; mutable files : t list }

  val compare : t -> t -> int
  val csv_count : int -> t -> int

  val get_custom_tree :
    order:[ `Random of Random.State.t | `Sorted | `Undefined ] ->
    tree:Store.tree ->
    where:string list ->
    t Lwt.t

  val file_names :
    order:[ `Random of Random.State.t | `Sorted | `Undefined ] ->
    tree:Store.tree ->
    where:string list ->
    string list Lwt.t
end = struct
  type t =
    | File of { name : string; path : Fpath.t; content : string }
    | Folder of { name : string; path : Fpath.t; mutable files : t list }

  let compare a b =
    match (a, b) with
    | (Folder _ as f1), (Folder _ as f2) when f1 = f2 -> 0
    | Folder { path = path1; _ }, Folder { path = path2; _ }
    | File { path = path1; _ }, File { path = path2; _ }
      when path1 > path2 ->
        1
    | Folder { path = path1; _ }, Folder { path = path2; _ }
    | File { path = path1; _ }, File { path = path2; _ }
      when path1 < path2 ->
        -1
    | Folder _, File _ -> 1
    | File _, Folder _ -> -1
    | _ -> 0

  let rec csv_count acc = function
    | File { path; _ } -> if Fpath.has_ext "csv" path then 1 else 0
    | Folder { files; _ } ->
        files |> List.map (csv_count 0) |> List.fold_left ( + ) acc

  let rec parents path acc =
    if Fpath.is_current_dir path then acc
    else
      let p = Fpath.parent path in
      parents p (Fpath.(p |> normalize) :: acc)

  let rec find_folder p x =
    match x with
    | File _ -> None
    | Folder { path; _ } as f when path = p -> Some f
    | Folder { files; _ } -> (
        let folders = List.map (find_folder p) files in
        List.filter_map (fun x -> x) folders |> function
        (* There can only be one folder with this path *)
        | [ f ] -> Some f
        | _ -> None)

  let get_t prefix path t (acc : t) =
    let fpath = String.concat "/" (prefix @ path) |> Fpath.v in
    if path = [] then acc |> Lwt.return
    else
      let ps = parents fpath [] |> List.rev in
      let parent = List.hd ps |> Fpath.rem_empty_seg in
      let name = Fpath.base fpath |> Fpath.to_string in
      Tree.kind t [] >>= function
      | Some k' -> (
          match k' with
          | `Node ->
              (* if we find the parent folder in the accumulator,
                 we can create a new folder inside of it *)
              (match find_folder parent acc with
              | Some (Folder f) ->
                  f.files <-
                    Folder { name; path = fpath; files = [] } :: f.files;
                  acc
              | _ -> acc)
              |> Lwt.return
          | `Contents -> (
              (* if we find the parent folder in the accumulator,
                 we can create a new file inside of it *)
              Tree.get t []
              >|= fun content ->
              match find_folder parent acc with
              | Some (Folder f) ->
                  f.files <- File { name; path = fpath; content } :: f.files;
                  acc
              | _ -> acc))
      | None -> failwith "no kind"

  let collect_list path t acc =
    let last = last_element path in
    let* k = Tree.kind t [] in
    match k with
    | Some k' ->
        (match k' with `Node -> acc | `Contents -> last :: acc) |> Lwt.return
    | None -> failwith "no kind"

  let get_custom_tree ~order ~tree ~where =
    let* tree = Tree.get_tree tree where in
    let name = String.concat "/" where in
    let path = name |> Fpath.v in
    Tree.fold ~order ~tree:(get_t where) tree
      (Folder { name; path; files = [] })

  let file_names ~order ~tree ~where =
    let* tree = Tree.get_tree tree where in
    Tree.fold ~order ~tree:collect_list tree [] >|= List.filter_map (fun x -> x)
end

let fetch ~ctx ~repo ~branch =
  Store.Backend.Remote.v repo >>= fun remote ->
  Store.Backend.Remote.fetch remote (ctx, addr) branch

let push ~ctx ~repo ~branch =
  Store.Backend.Remote.v repo >>= fun remote ->
  Store.Backend.Remote.push remote (ctx, addr) branch
