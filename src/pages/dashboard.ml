open Website_lib
open Tyxml

let color_from_counter n =
  let clamp x = if x < 0 then 0 else if x > 255 then 255 else x in
  let shade = clamp (255 - (n * 20)) in
  Printf.sprintf "#%02x%02x%02x" shade shade shade

let rec indent_from_counter n acc =
  if n = 0 then acc
  else indent_from_counter (n - 1) (Html.space () :: Html.space () :: acc)

let no_more_items_no_btn () =
  let open Tyxml.Html in
  div
    ~a:[ a_class [ "w-dyn-empty" ]; a_style "background-color: #d7d7d7" ]
    [
      div
        ~a:[ a_class [ "no-more-container" ] ]
        [ span [ "No more items found." |> txt ] ];
    ]

let no_more_items ~contrib path ~color_counter ~indent_counter =
  let open Tyxml.Html in
  div
    ~a:
      [
        a_class [ "w-dyn-empty" ];
        a_style ("background-color: " ^ color_from_counter (color_counter + 2));
      ]
    [
      div
        ~a:[ a_class [ "no-more-container" ] ]
        [
          span
            (indent_from_counter indent_counter
               [ "No more items found." |> txt ]);
          div ~a:[ a_class [ "no-more-spacer" ] ] [];
          div
            ~a:[ a_class [ "no-more-div" ] ]
            [
              a
                ~a:
                  [
                    a_href
                      ("/dashboard/add-file/" ^ path
                      |> Utils.contrib_query contrib);
                    a_class [ "no-more-button" ];
                  ]
                [ "new file" |> txt ];
              a
                ~a:
                  [
                    a_href
                      ("/dashboard/add-folder/" ^ path
                      |> Utils.contrib_query contrib);
                    a_class [ "no-more-button" ];
                  ]
                [ "new folder" |> txt ];
            ];
        ];
    ]

let rec tree_to_html ~contrib (tree : Files.Folder.t) (counter : int) () :
    _ Html.elt =
  let open Tyxml.Html in
  match tree with
  | File { name; path; _ } ->
      if name = "config" || name = ".ign" then a []
      else
        div
          ~a:[ a_class [ "file-div" ] ]
          [
            a
              ~a:
                [
                  a_href
                    ("/dashboard/" ^ Fpath.to_string path
                    |> Utils.contrib_query contrib);
                  a_class [ "file"; "w-dropdown-link" ];
                  a_style ("background-color: " ^ color_from_counter counter);
                ]
              (indent_from_counter counter
                 [ span ~a:[ a_class [ "file-text" ] ] [ name |> txt ] ]);
            div ~a:[ a_class [ "icon-file" ]; a_aria "hidden" [ "true" ] ] [];
          ]
  | Folder { name; path; files } ->
      let shortened_path =
        Fpath.rem_prefix (Fpath.v "inputs") path
        |> Option.value ~default:(Fpath.v name)
      in
      let files_html =
        (* if Files.Folder.csv_count 0 f = 0 then [] *)
        (* else *)
        files
        |> List.sort (fun a b -> Files.Folder.compare b a)
        |> List.map (fun file -> tree_to_html ~contrib file (counter + 1) ())
      in
      let files_html =
        files_html
        @ [
            no_more_items ~contrib
              (shortened_path |> Fpath.to_string)
              ~color_counter:(counter - 1) ~indent_counter:(counter + 2);
          ]
      in
      div
        ~a:
          [
            a_user_data "hover" "false";
            a_user_data "delay" "0";
            a_class [ "folder"; "w-dropdown" ];
          ]
        [
          div
            ~a:
              [
                a_class [ "folder-toggle"; "w-dropdown-toggle" ];
                a_id ("w-dropdown-toggle-" ^ string_of_int counter);
                a_aria "controls" [ "w-dropdown-list-" ^ string_of_int counter ];
                a_aria "haspopup" [ "menu" ];
                a_role [ "button" ];
                a_style ("background-color: " ^ color_from_counter counter);
                a_tabindex 0;
              ]
            [
              div
                [
                  a
                    ~a:
                      [
                        a_href
                          ("/dashboard/configs/"
                           ^ (shortened_path |> Fpath.to_string)
                          |> Utils.contrib_query contrib);
                        a_class [ "w-button"; "folder-button" ];
                      ]
                    [ "configure" |> txt ];
                  div
                    ~a:
                      [
                        a_class [ "w-icon-dropdown-toggle" ];
                        a_aria "hidden" [ "true" ];
                      ]
                    [];
                ];
              div
                ~a:[ a_class [ "folder-div" ] ]
                [
                  div
                    ~a:[ a_class [ "icon-folder" ]; a_aria "hidden" [ "true" ] ]
                    [];
                  div (indent_from_counter counter [ name |> txt ]);
                ];
            ];
          nav
            ~a:
              [
                a_class [ "files"; "w-dropdown-list" ];
                a_id ("w-dropdown-list-" ^ string_of_int counter);
                a_aria "labelledby"
                  [ "w-dropdown-toggle-" ^ string_of_int counter ];
              ]
            files_html;
        ]

let sidebar (user : User.t_no_pw) =
  General.sidebar "DASHBOARD" ("Welcome, " ^ user.full_name)

let dashboard_files ?(contrib = None) _request (inputs : Files.Folder.t) =
  let open Tyxml.Html in
  [
    div
      ~a:[ a_class [ "folders" ] ]
      begin
        match inputs with
        | Folder { files; _ } ->
            if List.length files = 0 then [ no_more_items_no_btn () ]
            else begin
              match List.hd files with
              | File { name = ".ign"; _ } -> [ no_more_items_no_btn () ]
              | _ ->
                  files
                  |> List.sort (fun a b -> Files.Folder.compare b a)
                  |> List.map (fun files -> tree_to_html ~contrib files 0 ())
            end
        | File _ -> assert false
      end;
  ]

let dashboard _request (user : User.t_no_pw) =
  let open Tyxml.Html in
  sidebar user
  @ [
      div
        ~a:[ a_class [ "_100vh" ] ]
        [
          div
            ~a:[ a_class [ "dashboard-container" ] ]
            [
              div
                ~a:[ a_class [ "dashboard-row"; "h40" ] ]
                [
                  div
                    ~a:[ a_class [ "db-panel"; "w75" ] ]
                    [
                      div
                        ~a:[ a_class [ "db-panel-container" ] ]
                        [
                          div
                            ~a:[ a_class [ "panel-head" ] ]
                            [
                              div
                                ~a:[ a_class [ "panel-icon-wrap" ] ]
                                [
                                  img ~alt:"" ~src:"/assets/icons/files.svg"
                                    ~a:[ a_class [ "panel-head-img" ] ]
                                    ();
                                ];
                              div
                                ~a:[ a_class [ "panel-name-wrap" ] ]
                                [
                                  div
                                    ~a:[ a_class [ "panel-name" ] ]
                                    [ txt "Files" ];
                                  div
                                    ~a:[ a_class [ "panel-filter" ] ]
                                    [ txt "for calibration" ];
                                ];
                              (if
                                 user.account_type = `Admin
                                 || user.account_type = `ElevatedUser
                               then
                                 a
                                   ~a:
                                     [
                                       a_href "/dashboard/add-file/";
                                       a_class [ "edit-button"; "w-button" ];
                                       a_style "padding-bottom: 14px;";
                                     ]
                                   [ txt "ADD FILE" ]
                               else a []);
                              (if
                                 user.account_type = `Admin
                                 || user.account_type = `ElevatedUser
                               then
                                 a
                                   ~a:
                                     [
                                       a_href "/dashboard/add-folder/";
                                       a_class [ "edit-button"; "w-button" ];
                                       a_style "padding-bottom: 14px;";
                                     ]
                                   [ txt "ADD FOLDER" ]
                               else a []);
                            ];
                          div
                            ~a:[ a_class [ "panel-body" ] ]
                            [
                              div
                                ~a:[ a_id "content" ]
                                [ (* this div is dynamic and will change with the laoder js script (ew) *) ];
                            ];
                        ];
                    ];
                ];
              div
                ~a:[ a_class [ "dashboard-row"; "h25" ] ]
                [
                  div
                    ~a:[ a_class [ "db-panel"; "w75" ] ]
                    [
                      div
                        ~a:[ a_class [ "db-panel-container" ] ]
                        [
                          div
                            ~a:[ a_class [ "panel-head" ] ]
                            [
                              div
                                ~a:[ a_class [ "panel-icon-wrap" ] ]
                                [
                                  img ~alt:""
                                    ~src:"/assets/icons/contributions.svg"
                                    ~a:[ a_class [ "panel-head-img" ] ]
                                    ();
                                ];
                              div
                                ~a:[ a_class [ "panel-name-wrap" ] ]
                                [
                                  div
                                    ~a:[ a_class [ "panel-name" ] ]
                                    [ txt "your contributions" ];
                                ];
                              a
                                ~a:
                                  [
                                    a_href "/dashboard/add-contrib";
                                    a_class [ "edit-button"; "w-button" ];
                                    a_style "padding-bottom: 14px;";
                                  ]
                                [ txt "CONTRIBUTE" ];
                            ];
                          div
                            ~a:[ a_class [ "panel-body" ] ]
                            [ div ~a:[ a_id "contribution_content" ] [] ];
                        ];
                    ];
                ];
            ];
        ];
    ]
