open Website_lib

(* also has remove folder button *)
let config_head request (user : User.t_no_pw) (config : Files_db.config)
    folder_path =
  let open Tyxml.Html in
  begin
    div
      ~a:[ a_class [ "panel-head" ] ]
      begin
        [
          div
            ~a:[ a_class [ "panel-icon-wrap" ] ]
            [
              img ~src:"/assets/icons/folder.svg" ~alt:""
                ~a:[ a_class [ "panel-head-img" ] ]
                ();
            ];
          div
            ~a:[ a_class [ "panel-name-wrap" ] ]
            [
              div
                ~a:[ a_class [ "panel-name" ] ]
                [ txt ("FOLDER " ^ folder_path) ];
            ];
        ]
        @
        if user.account_type = `Admin then
          [
            a
              ~a:
                [
                  a_class [ "edit-button"; "w-button" ];
                  a_href
                    (if config.typ = `Default then
                       "/dashboard/default_config/edit"
                     else "/dashboard/config/edit/" ^ folder_path);
                ]
              [ txt "EDIT CONFIG" ];
            (if config.typ = `Custom then
               form
                 ~a:
                   [
                     a_action ("/dashboard/config/remove/" ^ folder_path);
                     a_method `Post;
                   ]
                 [
                   Unsafe.data (Dream.csrf_tag request);
                   input
                     ~a:
                       [
                         a_input_type `Hidden; a_name "hidden"; a_value "hidden";
                       ]
                     ();
                   button
                     ~a:
                       [
                         a_class [ "edit-button"; "w-button" ];
                         a_style "padding-bottom: 14px;";
                       ]
                     [ txt "REMOVE CONFIG" ];
                 ]
             else a []);
            a
              ~a:
                [
                  a_class [ "edit-button"; "w-button" ];
                  a_href ("/dashboard/rename-folder/" ^ folder_path);
                ]
              [ txt "EDIT FOLDER" ];
            form
              ~a:
                [
                  a_action ("/dashboard/remove-folder/" ^ folder_path);
                  a_method `Post;
                ]
              [
                Unsafe.data (Dream.csrf_tag request);
                input
                  ~a:[ a_input_type `Hidden; a_name "hidden"; a_value "hidden" ]
                  ();
                button
                  ~a:
                    [
                      a_class [ "delete-button"; "w-button" ];
                      a_id "delete-button";
                      a_style "padding-bottom: 14px;";
                    ]
                  [ txt "DELETE FOLDER" ];
              ];
          ]
        else []
      end
  end

let config_details (config : Files_db.config) (folder : Files.Folder.t)
    (user : User.t_no_pw) request =
  let folder_path =
    match folder with Folder { path; _ } -> path | _ -> assert false
  in
  let folder_path =
    Fpath.rem_prefix (Fpath.v "inputs") folder_path
    |> Option.get |> Fpath.to_string
  in
  let open Tyxml.Html in
  [
    config_head request user config folder_path;
    div
      ~a:[ a_class [ "panel-body" ] ]
      [
        div
          ~a:
            [ a_class [ "w-layout-layout"; "quick-stack"; "wf-layout-layout" ] ]
          [
            div
              ~a:[ a_class [ "w-layout-cell"; "config-cell" ] ]
              [
                h4 [ txt "Details" ];
                div
                  ~a:[ a_class [ "config_desc" ] ]
                  [
                    txt
                      "Change the settings for the files processed inside this \
                       folder here.";
                    br ();
                    txt
                      "The configuration is specific to this folder. Any \
                       folders inside this one must have their own \
                       configurations, or use the default one.";
                  ];
                div ~a:[ a_class [ "separator" ] ] [];
                (if config.typ = `Default then
                   div
                     ~a:[ a_class [ "config-col"; "w-row" ] ]
                     [
                       div
                         ~a:
                           [
                             a_class
                               [ "config-column-left"; "w-col"; "w-col-10" ];
                           ]
                         [
                           div
                             ~a:[ a_class [ "config-value-left" ] ]
                             [
                               h5
                                 ~a:[ a_style "margin-top: 20px" ]
                                 [
                                   txt
                                     "This folder uses the default \
                                      configuration.";
                                 ];
                             ];
                         ];
                       (if user.account_type = `Admin then begin
                          div
                            ~a:
                              [
                                a_class
                                  [ "config-column-right"; "w-col"; "w-col-2" ];
                              ]
                            [
                              a
                                ~a:
                                  [
                                    a_href
                                      ("/dashboard/config/add/" ^ folder_path);
                                    a_style "\n  text-decoration: none;";
                                  ]
                                [
                                  div
                                    ~a:[ a_class [ "new-config-button" ] ]
                                    [ txt "Create Custom Configuration" ];
                                ];
                            ]
                        end
                        else a []);
                     ]
                 else
                   h5
                     ~a:[ a_style "margin-top: 20px" ]
                     [ txt "This folder has a custom configuration" ]);
                div ~a:[ a_class [ "separator" ] ] [];
                h4 [ txt "Values" ];
                div
                  ~a:[ a_class [ "config-div-in" ] ]
                  [
                    div
                      ~a:[ a_class [ "config-col"; "w-row" ] ]
                      [
                        div
                          ~a:
                            [
                              a_class
                                [ "config-column-left"; "w-col"; "w-col-10" ];
                            ]
                          [
                            div
                              ~a:[ a_class [ "config-value-left" ] ]
                              [ txt "Year Steps (for graphs)" ];
                          ];
                        div
                          ~a:
                            [
                              a_class
                                [ "config-column-right"; "w-col"; "w-col-2" ];
                            ]
                          [
                            div
                              ~a:[ a_class [ "config-value-right" ] ]
                              [ config.step |> string_of_int |> txt ];
                          ];
                      ];
                    div
                      ~a:[ a_class [ "config-col"; "w-row" ] ]
                      [
                        div
                          ~a:
                            [
                              a_class
                                [ "config-column-left"; "w-col"; "w-col-10" ];
                            ]
                          [
                            div
                              ~a:[ a_class [ "config-value-left" ] ]
                              [ txt "Confidence Interval" ];
                          ];
                        div
                          ~a:
                            [
                              a_class
                                [ "config-column-right"; "w-col"; "w-col-2" ];
                            ]
                          [
                            div
                              ~a:[ a_class [ "config-value-right" ] ]
                              [ config.confidence |> string_of_float |> txt ];
                          ];
                      ];
                  ];
                div ~a:[ a_class [ "separator" ] ] [];
                h4 [ txt "Filtering" ];
                div
                  ~a:[ a_class [ "config-div-in" ] ]
                  [
                    div
                      ~a:[ a_class [ "config-col"; "w-row" ] ]
                      [
                        div
                          ~a:
                            [
                              a_class
                                [ "config-column-left"; "w-col"; "w-col-10" ];
                            ]
                          [
                            div
                              ~a:[ a_class [ "config-value-left" ] ]
                              [ txt "Column" ];
                          ];
                        div
                          ~a:[ a_class [ "w-col"; "w-col-2" ] ]
                          [
                            div
                              ~a:[ a_class [ "config-value-right" ] ]
                              [
                                (match config.column with
                                | Some s -> txt s
                                | None -> txt "(nothing)");
                              ];
                          ];
                      ];
                    div
                      ~a:[ a_class [ "config-col"; "w-row" ] ]
                      [
                        div
                          ~a:
                            [
                              a_class
                                [ "config-column-left"; "w-col"; "w-col-10" ];
                            ]
                          [
                            div
                              ~a:[ a_class [ "config-value-left" ] ]
                              [ txt "Row" ];
                          ];
                        div
                          ~a:
                            [
                              a_class
                                [ "config-column-right"; "w-col"; "w-col-2" ];
                            ]
                          [
                            div
                              ~a:[ a_class [ "config-value-right" ] ]
                              [
                                (match config.value with
                                | Some s -> txt s
                                | None -> txt "(nothing)");
                              ];
                          ];
                      ];
                  ];
                div ~a:[ a_class [ "separator" ] ] [];
                h4 [ txt "Script" ];
                div
                  ~a:[ a_class [ "config-div-in" ] ]
                  [
                    div
                      ~a:[ a_class [ "config-col"; "w-row" ] ]
                      [
                        div
                          ~a:
                            [
                              a_class
                                [ "config-column-left"; "w-col"; "w-col-10" ];
                            ]
                          [
                            div
                              ~a:[ a_class [ "config-value-left" ] ]
                              [ txt "Name" ];
                          ];
                        div
                          ~a:
                            [
                              a_class
                                [ "config-column-right"; "w-col"; "w-col-2" ];
                            ]
                          [
                            div
                              ~a:[ a_class [ "config-value-right" ] ]
                              [ config.script |> txt ];
                          ];
                      ];
                  ];
              ];
          ];
      ];
  ]

let config _request =
  let open Tyxml.Html in
  General.sidebar "CONFIGURATION" ""
  @ [
      div
        ~a:[ a_class [ "_100vh" ] ]
        [
          div
            ~a:[ a_class [ "dashboard-container" ] ]
            [
              div
                ~a:[ a_class [ "dashboard-row"; "h100" ] ]
                [
                  div
                    ~a:[ a_class [ "db-panel"; "w75" ] ]
                    [
                      div
                        ~a:[ a_class [ "db-panel-container" ] ]
                        [
                          div
                            ~a:[ a_id "content" ]
                            [ (* this div is dynamic and will change with the laoder js script (ew) *) ];
                        ];
                    ];
                ];
            ];
        ];
    ]

let edit_content (folder : Files.Folder.t) (scripts : string list)
    (config : Files_db.config) request =
  let folder_path =
    match folder with Folder { path; _ } -> path | _ -> assert false
  in
  let folder_path =
    if folder_path = Fpath.v "." then "default config"
    else
      Fpath.rem_prefix (Fpath.v "inputs") folder_path
      |> Option.get |> Fpath.to_string
  in
  let open Tyxml.Html in
  [
    div
      ~a:[ a_class [ "panel-head" ] ]
      [
        div
          ~a:[ a_class [ "panel-icon-wrap" ] ]
          [
            img ~src:"/assets/icons/folder.svg" ~alt:""
              ~a:[ a_class [ "panel-head-img" ] ]
              ();
          ];
        div
          ~a:[ a_class [ "panel-name-wrap" ] ]
          [
            div
              ~a:[ a_class [ "panel-name" ] ]
              [
                txt
                  ((if config.typ = `Custom then "FOLDER " else "")
                  ^ folder_path);
              ];
          ];
      ];
    div
      ~a:[ a_class [ "panel-body" ] ]
      [
        div
          ~a:
            [ a_class [ "w-layout-layout"; "quick-stack"; "wf-layout-layout" ] ]
          [
            (if config.typ = `Default then
               h5 [ txt "CAREFUL! You are editing the DEFAULT configuration" ]
             else a []);
            (if config.typ = `Default then div ~a:[ a_class [ "separator" ] ] []
             else a []);
            div
              ~a:[ a_class [ "form-block-2"; "w-form" ] ]
              [
                form
                  ~a:
                    [
                      a_id "config-form";
                      a_name "config-form";
                      a_method `Post;
                      a_class [ "form-2" ];
                      a_aria "label" [ "Email Form" ];
                      a_action
                        (if config.typ = `Custom then
                           "/dashboard/config/edit/" ^ folder_path
                         else "/dashboard/default_config/edit");
                    ]
                  [
                    Unsafe.data (Dream.csrf_tag request);
                    h4 [ txt "Values" ];
                    label ~a:[ a_label_for "step" ] [ txt "Year Steps" ];
                    input
                      ~a:
                        [
                          a_class [ "user-edit-form-text-field"; "w-input" ];
                          a_maxlength 256;
                          a_name "step";
                          a_value (config.step |> string_of_int);
                          a_input_type `Number;
                          a_id "step";
                          a_required ();
                        ]
                      ();
                    label
                      ~a:[ a_label_for "confidence" ]
                      [ txt "Confidence Interval" ];
                    input
                      ~a:
                        [
                          a_class [ "user-edit-form-text-field"; "w-input" ];
                          a_maxlength 256;
                          a_name "confidence";
                          a_value (config.confidence |> string_of_float);
                          a_input_type `Number;
                          a_step (Some 0.01);
                          a_id "confidence";
                          a_required ();
                        ]
                      ();
                    h4 [ txt "Filtering" ];
                    div
                      ~a:[ a_class [ "text-block-15" ] ]
                      [
                        txt
                          "You can either have no filtering (empty fields), or \
                           filtering on a specific column and row (both fields \
                           filled).";
                      ];
                    label ~a:[ a_label_for "column" ] [ txt "Column" ];
                    input
                      ~a:
                        [
                          a_class [ "user-edit-form-text-field"; "w-input" ];
                          a_maxlength 256;
                          a_name "column";
                          begin
                            match config.column with
                            | Some s -> a_value s
                            | None -> a_placeholder "Material"
                          end;
                          a_input_type `Text;
                          a_id "column";
                        ]
                      ();
                    label ~a:[ a_label_for "value" ] [ txt "Row" ];
                    input
                      ~a:
                        [
                          a_class [ "user-edit-form-text-field"; "w-input" ];
                          a_maxlength 256;
                          a_name "value";
                          begin
                            match config.value with
                            | Some s -> a_value s
                            | None -> a_placeholder "Bone"
                          end;
                          a_input_type `Text;
                          a_id "value";
                        ]
                      ();
                    h4 [ txt "Script" ];
                    select
                      ~a:
                        [
                          a_id "script";
                          a_name "script";
                          a_required ();
                          a_class [ "select-field"; "w-select" ];
                        ]
                      (List.map
                         (fun script ->
                           option
                             ~a:
                               (if script = config.script then
                                  [ a_value script; a_selected () ]
                                else [ a_value script ])
                             (txt script))
                         scripts);
                    input
                      ~a:
                        [
                          a_input_type `Submit;
                          a_class [ "submit-button"; "w-button" ];
                          a_value "Submit";
                        ]
                      ();
                  ];
              ];
          ];
      ];
  ]

let edit _request =
  let open Tyxml.Html in
  General.sidebar "CONFIGURATION" ""
  @ [
      div
        ~a:[ a_class [ "_100vh" ] ]
        [
          div
            ~a:[ a_class [ "dashboard-container" ] ]
            [
              div
                ~a:[ a_class [ "dashboard-row"; "h100" ] ]
                [
                  div
                    ~a:[ a_class [ "db-panel"; "w75" ] ]
                    [
                      div
                        ~a:[ a_class [ "db-panel-container" ] ]
                        [
                          div
                            ~a:[ a_id "content" ]
                            [ (* this div is dynamic and will change with the laoder js script (ew) *) ];
                        ];
                    ];
                ];
            ];
        ];
    ]

let add (folder : Files.Folder.t) (scripts : string list) request =
  let folder_path =
    match folder with Folder { path; _ } -> path | _ -> assert false
  in
  let folder_path =
    if folder_path = Fpath.v "." then "default config"
    else
      Fpath.rem_prefix (Fpath.v "inputs") folder_path
      |> Option.get |> Fpath.to_string
  in
  let open Tyxml.Html in
  General.sidebar "CONFIGURATION" ""
  @ [
      div
        ~a:[ a_class [ "_100vh" ] ]
        [
          div
            ~a:[ a_class [ "dashboard-container" ] ]
            [
              div
                ~a:[ a_class [ "dashboard-row"; "h100" ] ]
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
                                  img ~src:"/assets/icons/folder.svg" ~alt:""
                                    ~a:[ a_class [ "panel-head-img" ] ]
                                    ();
                                ];
                              div
                                ~a:[ a_class [ "panel-name-wrap" ] ]
                                [
                                  div
                                    ~a:[ a_class [ "panel-name" ] ]
                                    [ txt ("FOLDER " ^ folder_path) ];
                                ];
                            ];
                          div
                            ~a:[ a_class [ "panel-body" ] ]
                            [
                              div
                                ~a:
                                  [
                                    a_class
                                      [
                                        "w-layout-layout";
                                        "quick-stack";
                                        "wf-layout-layout";
                                      ];
                                  ]
                                [
                                  div
                                    ~a:[ a_class [ "form-block-2"; "w-form" ] ]
                                    [
                                      form
                                        ~a:
                                          [
                                            a_id "config-form";
                                            a_name "config-form";
                                            a_method `Post;
                                            a_class [ "form-2" ];
                                            a_aria "label" [ "Email Form" ];
                                            a_action
                                              ("/dashboard/config/add/"
                                             ^ folder_path);
                                          ]
                                        [
                                          Unsafe.data (Dream.csrf_tag request);
                                          h4 [ txt "Values" ];
                                          label
                                            ~a:[ a_label_for "step" ]
                                            [ txt "Year Steps" ];
                                          input
                                            ~a:
                                              [
                                                a_class
                                                  [
                                                    "user-edit-form-text-field";
                                                    "w-input";
                                                  ];
                                                a_maxlength 256;
                                                a_name "step";
                                                a_input_type `Number;
                                                a_id "step";
                                                a_required ();
                                              ]
                                            ();
                                          label
                                            ~a:[ a_label_for "confidence" ]
                                            [ txt "Confidence Interval" ];
                                          input
                                            ~a:
                                              [
                                                a_class
                                                  [
                                                    "user-edit-form-text-field";
                                                    "w-input";
                                                  ];
                                                a_maxlength 256;
                                                a_name "confidence";
                                                a_input_type `Number;
                                                a_step (Some 0.01);
                                                a_id "confidence";
                                                a_required ();
                                              ]
                                            ();
                                          h4 [ txt "Filtering" ];
                                          div
                                            ~a:[ a_class [ "text-block-15" ] ]
                                            [
                                              txt
                                                "You can either have no \
                                                 filtering (empty fields), or \
                                                 filtering on a specific \
                                                 column and row (both fields \
                                                 filled).";
                                            ];
                                          label
                                            ~a:[ a_label_for "column" ]
                                            [ txt "Column" ];
                                          input
                                            ~a:
                                              [
                                                a_class
                                                  [
                                                    "user-edit-form-text-field";
                                                    "w-input";
                                                  ];
                                                a_maxlength 256;
                                                a_name "column";
                                                a_input_type `Text;
                                                a_id "column";
                                              ]
                                            ();
                                          label
                                            ~a:[ a_label_for "value" ]
                                            [ txt "Row" ];
                                          input
                                            ~a:
                                              [
                                                a_class
                                                  [
                                                    "user-edit-form-text-field";
                                                    "w-input";
                                                  ];
                                                a_maxlength 256;
                                                a_name "value";
                                                a_input_type `Text;
                                                a_id "value";
                                              ]
                                            ();
                                          h4 [ txt "Script" ];
                                          select
                                            ~a:
                                              [
                                                a_id "script";
                                                a_name "script";
                                                a_required ();
                                                a_class
                                                  [ "select-field"; "w-select" ];
                                              ]
                                            (List.map
                                               (fun script ->
                                                 option
                                                   ~a:[ a_value script ]
                                                   (txt script))
                                               scripts);
                                          input
                                            ~a:
                                              [
                                                a_input_type `Submit;
                                                a_class
                                                  [
                                                    "submit-button"; "w-button";
                                                  ];
                                                a_value "Submit";
                                              ]
                                            ();
                                        ];
                                    ];
                                ];
                            ];
                        ];
                    ];
                ];
            ];
        ];
    ]
