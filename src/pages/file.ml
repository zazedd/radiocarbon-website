open Website_lib
open Tyxml.Html

let output_file (name, p) =
  li
    ~a:[ a_role [ "listitem" ] ]
    [
      a
        ~a:
          [
            a_href ("/dashboard/get-output/" ^ Fpath.to_string p);
            a_class [ "w-layout-grid"; "output-file-grid" ];
          ]
        [
          div ~a:[ a_class [ "card-lable" ] ] [ txt "SCRIPT" ];
          div
            ~a:[ a_class [ "card-info-text"; "bold"; "output-file-name" ] ]
            [ txt name ];
        ];
    ]

let output_pdf name =
  let typ, date, p = name in
  li
    ~a:[ a_role [ "listitem" ]; a_class [ "pdf-list-item" ] ]
    [
      div
        ~a:[ a_class [ "pdf-list-div" ] ]
        [
          a
            ~a:
              [
                a_href ("/dashboard/get-output/" ^ Fpath.to_string p);
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

let csv (data : string list list) =
  let row (row_data : string list) =
    tr (List.map (fun cell_data -> td [ txt cell_data ]) row_data)
  in
  table ~a:[ a_class [ "testing" ] ] (List.map row data)

let file_details path (file : Files.File.t) request =
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
                    div
                      ~a:[ a_class [ "csv-table" ] ]
                      [ file.content |> Files.Csv.parse_csv |> csv ];
                  ];
              ];
            div
              ~a:[ a_class [ "card-right-wrap" ] ]
              [
                div
                  ~a:[ a_class [ "card-multi-images" ] ]
                  [
                    div
                      ~a:[ a_class [ "card-lable"; "inline" ] ]
                      [ txt "CSV OUTPUTS" ];
                    ul
                      ~a:[ a_role [ "list" ]; a_class [ "output-file-list" ] ]
                      (List.map output_file file.outputs);
                  ];
                div
                  ~a:[ a_class [ "card-info-row"; "mobile-space" ] ]
                  [ div ~a:[ a_class [ "card-info-wrap" ] ] [] ];
                div
                  ~a:[ a_class [ "card-multi-images" ] ]
                  [
                    div
                      ~a:[ a_class [ "card-lable"; "inline" ] ]
                      [ txt "PLOT PDF OUTPUTS" ];
                    ul
                      ~a:
                        [
                          a_role [ "list" ];
                          a_class [ "pdf-list w-list-unstyled" ];
                        ]
                      (List.map output_pdf file.pdfs);
                  ];
                div
                  ~a:[ a_class [ "card-info-row"; "mobile-space" ] ]
                  [ div ~a:[ a_class [ "card-info-wrap" ] ] [] ];
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
                                  a_class [ "delete-button-file"; "w-button" ];
                                  a_style
                                    "padding-bottom: 14px; border-radius: 5px";
                                ]
                              [ txt "DELETE" ];
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
