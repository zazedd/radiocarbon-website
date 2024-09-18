open Lwt.Syntax
open Lwt.Infix

let rp = open_in "env/repo.txt" |> input_line |> String.trim
let addr = Smart_git.Endpoint.of_string rp |> Result.get_ok

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
  val parse : string -> string list list
  val sanitize : string -> string
end = struct
  let parse str =
    let lines = String.split_on_char '\n' str in
    let rec h c line =
      let line = String.trim line in
      if line = "" then []
      else begin
        match String.split_on_char c line with
        | [ "" ] -> []
        | [ _ ] | [] ->
            h ';' line (* if the delimiter is not ',' we can try ';' *)
        | lst -> lst
      end
    in
    List.map (h ',') lines |> List.filter (( <> ) [])

  let sanitize str =
    let lines = parse str in
    (List.filter
       (fun (col_values : string list) ->
         List.fold_left
           (fun acc col_value -> acc || not (col_value = ""))
           false col_values)
       lines
    |> List.map (String.concat ",")
       (* add a last empty column such that R can properly read the header
          (R is weird) *)
    |> List.map (fun s -> s ^ ",")
    |> String.concat "\n")
    ^ "\n"
end

let remove_dot f = String.sub f 1 (String.length f - 1)

type output =
  [ `Pdf of string * string * Fpath.t | `Csv of string * string * Fpath.t ]
(* csv -> (script name, date, file path) *)
(* pdf -> (type of pdf, date, file path) *)

module File : sig
  type t = {
    name : Fpath.t;
    content : string;
    outputs : (Digestif.SHA1.t, output list) Hashtbl.t;
  }

  val get_file :
    t:Store.t ->
    in_where:string list ->
    out_where:Store.path ->
    name:string ->
    t Lwt.t
end = struct
  type t = {
    name : Fpath.t;
    content : string;
    (* git hash -> outputs *)
    outputs : (Digestif.SHA1.t, output list) Hashtbl.t;
  }

  let compare_output o1 o2 =
    let date_of_output = function
      | `Pdf (_, date, _) | `Csv (_, date, _) -> date
    in
    compare (date_of_output o2) (date_of_output o1)

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
    let date = remove_dot date_dot and script = remove_dot script_dot in
    (script, date, out_path)

  (* inputname.script.type.date.pdf *)
  let pdf_name fpath out_file =
    let out_path = Fpath.(fpath // out_file) in
    let date_dot = out_file |> Fpath.rem_ext |> Fpath.get_ext
    and typ_dot = out_file |> Fpath.rem_ext |> Fpath.rem_ext |> Fpath.get_ext in
    let date = remove_dot date_dot and typ = remove_dot typ_dot in
    (typ, date, out_path)

  let collect_list ~head ~out_where p (acc : t) (path, t) =
    let store_output output =
      let+ commit =
        Store.last_modified head (out_where @ [ path ]) >|= fun x -> List.hd x
      in
      let hash = Store.Commit.hash commit in
      begin
        match Hashtbl.find_opt acc.outputs hash with
        | Some l ->
            Hashtbl.replace acc.outputs hash (output :: l);
            acc
        | None ->
            Hashtbl.add acc.outputs hash [ output ];
            acc
      end
    in
    if path = "" then acc |> Lwt.return
    else
      let fpath = path |> Fpath.v in
      let name = fpath |> Fpath.base in
      let* k = Tree.kind t [] in
      match k with
      | Some k' -> (
          match k' with
          | `Node -> acc |> Lwt.return
          | `Contents -> begin
              if is_output acc.name name then
                store_output (`Csv (name |> script_name p))
              else if is_pdf_output acc.name name then
                store_output (`Pdf (name |> pdf_name p))
              else acc |> Lwt.return
            end)
      | None -> failwith "no kind"

  let get_file ~t ~in_where ~out_where ~name =
    let* tree = Store.tree t in
    let out_path =
      match List.tl out_where with
      | [] -> Fpath.v "."
      | lst -> lst |> String.concat "/" |> Fpath.v
    in
    let* content = Tree.get tree (in_where @ [ name ]) in
    let fname = Fpath.v name in
    let* tree = Tree.get_tree tree out_where in
    Tree.list tree [] >>= fun l ->
    Lwt_list.fold_left_s
      (collect_list ~head:t ~out_where out_path)
      { name = fname; content; outputs = Hashtbl.create 0 }
      l
    >|= fun file ->
    Hashtbl.iter
      (fun k v ->
        let l = List.sort compare_output v in
        Hashtbl.replace file.outputs k l)
      file.outputs;
    file
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
  Logs.info (fun f -> f "Fetching from %s:%s" rp branch);
  Store.Backend.Remote.fetch remote (ctx, addr) branch

let push ~ctx ~repo ~branch =
  Store.Backend.Remote.v repo >>= fun remote ->
  Logs.info (fun f -> f "Pushing to %s:%s" rp branch);
  Store.Backend.Remote.push remote (ctx, addr) branch

let fetch_all ~ctx ~repo =
  Store.Backend.Remote.v repo >>= fun remote ->
  Logs.info (fun f -> f "Fetching everyone from %s" rp);
  Store.Backend.Remote.fetch_all remote (ctx, addr) >|= fun lst ->
  let errs = List.filter (function Error _ -> true | Ok _ -> false) lst in
  match errs with
  | [] ->
      Ok
        (lst
        |> List.filter_map (function
             | Ok commit -> Some commit
             | Error _ -> None))
  | (Error _ as err) :: _ -> err
  | _ -> assert false
