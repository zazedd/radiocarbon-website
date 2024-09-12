open Website_lib
open Tyxml.Html

let output_config ?(single = false) (config : Files_db.config) =
  div
    ~a:[ a_class [ "w-layout-cell config-cell" ] ]
    [
      div
        [
          (if single then
             txt "This output was generated with this configuration:"
           else
             txt "The following outputs were generated with this configuration:");
        ];
      h4 [ txt "Values" ];
      div
        ~a:[ a_class [ "config-div-in" ] ]
        [
          div
            ~a:[ a_class [ "config-col w-row" ] ]
            [
              div
                ~a:[ a_class [ "config-column-left w-col w-col-9" ] ]
                [
                  div
                    ~a:[ a_class [ "config-value-left" ] ]
                    [ txt "Year Steps (for graphs)" ];
                ];
              div
                ~a:[ a_class [ "config-column-right w-col w-col-3" ] ]
                [
                  div
                    ~a:[ a_class [ "config-value-right" ] ]
                    [ txt (config.step |> string_of_int) ];
                ];
            ];
          div
            ~a:[ a_class [ "config-col w-row" ] ]
            [
              div
                ~a:[ a_class [ "config-column-left w-col w-col-9" ] ]
                [
                  div
                    ~a:[ a_class [ "config-value-left" ] ]
                    [ txt "Confidence Interval" ];
                ];
              div
                ~a:[ a_class [ "config-column-right w-col w-col-3" ] ]
                [
                  div
                    ~a:[ a_class [ "config-value-right" ] ]
                    [ txt (config.confidence |> string_of_float) ];
                ];
            ];
        ];
      h4 [ txt "Filtering" ];
      div
        ~a:[ a_class [ "config-div-in" ] ]
        [
          div
            ~a:[ a_class [ "config-col w-row" ] ]
            [
              div
                ~a:[ a_class [ "config-column-left w-col w-col-8" ] ]
                [ div ~a:[ a_class [ "config-value-left" ] ] [ txt "Column" ] ];
              div
                ~a:[ a_class [ "config-column-right w-col w-col-4" ] ]
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
            ~a:[ a_class [ "config-col w-row" ] ]
            [
              div
                ~a:[ a_class [ "config-column-left w-col w-col-8" ] ]
                [ div ~a:[ a_class [ "config-value-left" ] ] [ txt "Row" ] ];
              div
                ~a:[ a_class [ "config-column-right w-col w-col-4" ] ]
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
    ]

let output_file (name, date, p) =
  li
    ~a:[ a_role [ "listitem" ] ]
    [
      a
        ~a:
          [
            a_href ("/dashboard/outputs/" ^ Fpath.to_string p);
            a_class [ "w-layout-grid"; "output-file-grid" ];
          ]
        [
          div ~a:[ a_class [ "card-lable" ] ] [ txt "SCRIPT" ];
          div
            ~a:[ a_class [ "card-info-text"; "bold"; "output-file-name" ] ]
            [ txt name ];
          div ~a:[ a_class [ "card-lable" ] ] [ txt "DATE" ];
          div
            ~a:[ a_class [ "card-info-text"; "bold"; "output-file-name" ] ]
            [ txt date ];
        ];
    ]

let output_pdf (typ, date, p) =
  li
    ~a:[ a_role [ "listitem" ]; a_class [ "pdf-list-item" ] ]
    [
      div
        ~a:[ a_class [ "pdf-list-div" ] ]
        [
          a
            ~a:
              [
                a_href ("/dashboard/outputs/" ^ Fpath.to_string p);
                a_class [ "w-layout-grid"; "pdf-list-grid" ];
              ]
            [
              div ~a:[ a_class [ "card-lable" ] ] [ txt "TYPE" ];
              div
                ~a:[ a_class [ "card-info-text"; "bold"; "output-file-name" ] ]
                [ txt typ ];
              div ~a:[ a_class [ "card-lable" ] ] [ txt "DATE" ];
              div
                ~a:[ a_class [ "card-info-text"; "bold"; "output-file-name" ] ]
                [ txt date ];
            ];
        ];
    ]

let html_outputs (file : Files.File.t)
    (configs : (Digestif.SHA1.t, Files_db.config) Hashtbl.t) =
  Hashtbl.fold
    (fun k v acc ->
      match Hashtbl.find_opt configs k with
      | Some config ->
          let csvs =
            List.filter_map (function `Csv x -> Some x | _ -> None) v
          in
          let pdfs =
            List.filter_map (function `Pdf x -> Some x | _ -> None) v
          in
          let _, date, _ =
            if List.length csvs <> 0 then List.hd csvs else List.hd pdfs
          in
          let html =
            [
              output_config config;
              div
                ~a:[ a_class [ "card-lable-2"; "inline" ] ]
                [ txt "CSV OUTPUTS" ];
              ul
                ~a:[ a_role [ "list" ]; a_class [ "output-file-list" ] ]
                (List.map output_file csvs);
              div
                ~a:[ a_class [ "card-info-row"; "mobile-space" ] ]
                [ div ~a:[ a_class [ "card-info-wrap" ] ] [] ];
              div
                ~a:[ a_class [ "card-multi-images" ] ]
                [
                  div
                    ~a:[ a_class [ "card-lable-2"; "inline" ] ]
                    [ txt "PLOT PDF OUTPUTS" ];
                  ul
                    ~a:
                      [
                        a_role [ "list" ];
                        a_class [ "pdf-list w-list-unstyled" ];
                      ]
                    (List.map output_file pdfs);
                ];
            ]
          in
          (date, html) :: acc
      | None -> acc)
    file.outputs []
  |> List.sort (fun (date1, _) (date2, _) -> compare date2 date1)
  |> List.map snd

let csv (data : string list list) =
  let row (row_data : string list) =
    tr (List.map (fun cell_data -> td [ txt cell_data ]) row_data)
  in
  table ~a:[ a_class [ "csv-cells" ] ] (List.map row data)

let file_details path (file : Files.File.t)
    (configs : (Digestif.SHA1.t, Files_db.config) Hashtbl.t) request =
  let first_outputs, other_outputs =
    if Hashtbl.length file.outputs = 0 then
      ( [
          div
            [
              txt
                "No outputs yet! Wait until the pipeline finishes and check \
                 again.";
            ];
        ],
        [] )
    else
      let outputs = html_outputs file configs in
      ( List.hd outputs,
        List.tl outputs
        |> List.map (fun x -> x @ [ div ~a:[ a_class [ "separator" ] ] [] ])
        |> List.flatten )
  in

  [
    div
      ~a:[ a_class [ "card-table" ] ]
      [
        div
          ~a:[ a_class [ "file-header" ] ]
          [
            div
              ~a:[ a_class [ "card-header-wrap" ] ]
              [
                div
                  ~a:[ a_class [ "header-desc" ] ]
                  [
                    div
                      ~a:[ a_class [ "header-title" ] ]
                      [ txt (file.name |> Fpath.to_string) ];
                  ];
              ];
          ];
        div
          ~a:[ a_class [ "card-wrapper" ] ]
          [
            div
              ~a:[ a_class [ "card-left-wrap" ] ]
              [
                div
                  ~a:[ a_class [ "card-main-info" ] ]
                  [
                    h4
                      ~a:[ a_class [ "csv-header" ] ]
                      [ txt "Current file content:" ];
                    div
                      ~a:[ a_class [ "csv-table" ] ]
                      [ file.content |> Files.Csv.parse |> csv ];
                  ];
              ];
            div
              ~a:[ a_class [ "card-right-wrap" ] ]
              [
                div
                  ~a:[ a_class [ "w-layout-layout"; "wf-layout-layout" ] ]
                  [
                    div ~a:[ a_class [ "card-multi-images" ] ] first_outputs;
                    (if List.length other_outputs <> 0 then
                       div
                         ~a:[ a_class [ "output-file-dropdown w-dropdown" ] ]
                         [
                           div
                             ~a:
                               [
                                 a_class
                                   [ "dropdown-toggle-3 w-dropdown-toggle" ];
                               ]
                             [
                               div
                                 ~a:[ a_class [ "w-icon-dropdown-toggle" ] ]
                                 [];
                               div [ txt "Older Outputs" ];
                             ];
                           nav
                             ~a:
                               [
                                 a_class
                                   [
                                     "output-file-dropdownlist w-dropdown-list";
                                   ];
                               ]
                             [
                               div
                                 ~a:[ a_class [ "inner-dropdown-div-output" ] ]
                                 [
                                   div
                                     ~a:[ a_class [ "output-inner-dropdown" ] ]
                                     other_outputs;
                                 ];
                             ];
                         ]
                     else div []);
                    div ~a:[ a_class [ "separator" ] ] [];
                    div
                      ~a:[ a_class [ "card-multi-images" ] ]
                      [
                        div
                          ~a:[ a_class [ "card-lable"; "inline" ] ]
                          [ txt "OTHER" ];
                        div
                          ~a:[ a_class [ "file-button-container" ] ]
                          [
                            a
                              ~a:
                                [
                                  a_href ("/dashboard/edit-file/" ^ path);
                                  a_class [ "edit-file-button"; "w-button" ];
                                ]
                              [ txt "EDIT" ];
                            form
                              ~a:
                                [
                                  a_action ("/dashboard/remove-file/" ^ path);
                                  a_method `Post;
                                ]
                              [
                                Unsafe.data (Dream.csrf_tag request);
                                input
                                  ~a:
                                    [
                                      a_input_type `Hidden;
                                      a_name "hidden";
                                      a_value "hidden";
                                    ]
                                  ();
                                button
                                  ~a:
                                    [
                                      a_id "delete-button";
                                      a_class
                                        [ "delete-button-file"; "w-button" ];
                                      a_style
                                        "padding-bottom: 14px; border-radius: \
                                         5px";
                                    ]
                                  [ txt "DELETE" ];
                              ];
                          ];
                      ];
                  ];
              ];
          ];
      ];
  ]

let file _request =
  General.sidebar "FILE" ""
  @ [
      div
        ~a:[ a_class [ "_100vh" ] ]
        [
          div
            ~a:[ a_class [ "card-container" ] ]
            [
              div
                ~a:[ a_class [ "card-table" ] ]
                [ div ~a:[ a_id "content" ] [] ];
            ];
        ];
    ]

let add folder_path request =
  let folder_path_str = if folder_path = "" then "/" else folder_path in
  General.sidebar "FILE" ""
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
                                  img ~src:"/assets/icons/file.svg" ~alt:""
                                    ~a:[ a_class [ "panel-head-img" ] ]
                                    ();
                                ];
                              div
                                ~a:[ a_class [ "panel-name-wrap" ] ]
                                [
                                  div
                                    ~a:[ a_class [ "panel-name" ] ]
                                    [ txt ("ADD FILE IN " ^ folder_path_str) ];
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
                                            a_id "file-form";
                                            a_name "file-form";
                                            a_method `Post;
                                            a_class [ "form-2" ];
                                            a_aria "label" [ "Email Form" ];
                                            a_action
                                              ("/dashboard/add-file/"
                                             ^ folder_path);
                                            a_enctype "multipart/form-data";
                                          ]
                                        [
                                          Unsafe.data (Dream.csrf_tag request);
                                          label
                                            ~a:[ a_label_for "file" ]
                                            [ txt "File" ];
                                          input
                                            ~a:
                                              [
                                                a_class
                                                  [
                                                    "user-edit-form-text-field";
                                                    "w-input";
                                                  ];
                                                a_maxlength 256;
                                                a_name "file";
                                                a_input_type `File;
                                                a_id "file";
                                                a_required ();
                                              ]
                                            ();
                                          input
                                            ~a:
                                              [
                                                a_input_type `Submit;
                                                a_class
                                                  [
                                                    "submit-button-file-edit";
                                                    "w-button";
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

let edit_content path (file : Files.File.t) request =
  [
    div
      ~a:[ a_class [ "card-table" ] ]
      [
        div
          ~a:[ a_class [ "file-header" ] ]
          [
            div
              ~a:[ a_class [ "card-header-wrap" ] ]
              [
                div
                  ~a:[ a_class [ "header-desc" ] ]
                  [
                    div
                      ~a:[ a_class [ "header-title" ] ]
                      [ txt (file.name |> Fpath.to_string) ];
                  ];
              ];
          ];
        div
          ~a:[ a_class [ "card-wrapper" ] ]
          [
            div
              ~a:[ a_class [ "config-edit-div" ] ]
              [
                div
                  ~a:[ a_class [ "form-block"; "w-form" ] ]
                  [
                    form
                      ~a:
                        [
                          a_id "email-form";
                          a_name "config-form";
                          a_method `Post;
                          a_action ("/dashboard/edit-file/" ^ path);
                          a_class [ "form" ];
                        ]
                      [
                        Unsafe.data (Dream.csrf_tag request);
                        div
                          ~a:[ a_class [ "card-main-info" ] ]
                          [
                            div
                              ~a:[ a_class [ "w-row" ] ]
                              [
                                div
                                  ~a:
                                    [
                                      a_class
                                        [
                                          "csv-edit-column"; "w-col"; "w-col-6";
                                        ];
                                    ]
                                  [
                                    div
                                      ~a:[ a_class [ "file-edit-text" ] ]
                                      [
                                        txt
                                          "Edit the content of the file in the \
                                           block below:";
                                      ];
                                  ];
                                div
                                  ~a:
                                    [
                                      a_class
                                        [
                                          "submit-button-file-edit-div";
                                          "w-col";
                                          "w-col-6";
                                        ];
                                    ]
                                  [
                                    button
                                      ~a:
                                        [
                                          a_class
                                            [
                                              "submit-button-file-edit";
                                              "w-button";
                                            ];
                                          a_button_type `Submit;
                                        ]
                                      [ txt "Submit" ];
                                  ];
                              ];
                            div
                              ~a:[ a_class [ "csv-edit-content" ] ]
                              [
                                textarea
                                  ~a:
                                    [
                                      a_required ();
                                      a_placeholder "Example Text";
                                      a_maxlength 5000;
                                      a_id "config-edit-field";
                                      a_name "config-edit-field";
                                      a_class [ "csv-edit-textarea"; "w-input" ];
                                    ]
                                  (txt file.content);
                              ];
                          ];
                      ];
                  ];
              ];
          ];
      ];
  ]
