(* open Radiocarbon_lib *)

let add path request =
  let path = if path = "" then "/" else path in
  let open Tyxml.Html in
  General.sidebar "NEW FOLDER" ""
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
                                    [ txt ("NEW FOLDER IN " ^ path) ];
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
                                              ("/dashboard/add-folder/" ^ path);
                                          ]
                                        [
                                          Unsafe.data (Dream.csrf_tag request);
                                          h4 [ txt "New Folder" ];
                                          label
                                            ~a:[ a_label_for "name" ]
                                            [ txt "Name" ];
                                          input
                                            ~a:
                                              [
                                                a_class
                                                  [
                                                    "user-edit-form-text-field";
                                                    "w-input";
                                                  ];
                                                a_maxlength 256;
                                                a_name "name";
                                                a_input_type `Text;
                                                a_id "name";
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

let rename old_name path request =
  let open Tyxml.Html in
  General.sidebar "RENAME FOLDER" ""
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
                                    [ txt ("RENAME FOLDER " ^ path) ];
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
                                              ("/dashboard/rename-folder/"
                                             ^ path);
                                          ]
                                        [
                                          Unsafe.data (Dream.csrf_tag request);
                                          h4 [ txt "Rename Folder" ];
                                          label
                                            ~a:[ a_label_for "name" ]
                                            [ txt "Name" ];
                                          input
                                            ~a:
                                              [
                                                a_class
                                                  [
                                                    "user-edit-form-text-field";
                                                    "w-input";
                                                  ];
                                                a_maxlength 256;
                                                a_name "name";
                                                a_value old_name;
                                                a_input_type `Text;
                                                a_id "name";
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
