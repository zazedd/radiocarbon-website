open Radiocarbon_lib

let get_user_rows (users : User.t_no_pw list) =
  let get_user_row (user : User.t_no_pw) =
    let open Tyxml.Html in
    div
      ~a:[ a_role [ "listitem" ]; a_class [ "catalog-item"; "w-dyn-item" ] ]
      [
        a
          ~a:
            [
              a_href ("/dashboard/users/" ^ Base64.encode_string user.email);
              a_class [ "panel-row"; "w-inline-block" ];
            ]
          [
            div
              ~a:[ a_class [ "panel-col" ] ]
              [
                div
                  ~a:[ a_class [ "panel-col-text"; "no-wrap" ] ]
                  [ user.full_name |> txt ];
              ];
            div
              ~a:[ a_class [ "panel-col" ] ]
              [ div ~a:[ a_class [ "panel-col-text" ] ] [ user.email |> txt ] ];
            div
              ~a:[ a_class [ "panel-col" ] ]
              [
                div
                  ~a:[ a_class [ "panel-col-text" ] ]
                  [ User.account_type_to_string user.account_type |> txt ];
              ];
          ];
      ]
  in
  List.fold_left (fun acc user -> get_user_row user :: acc) [] users

let sidebar (user : User.t_no_pw) =
  General.sidebar "USERS" ("Welcome, " ^ user.full_name)

let users _request (user : User.t_no_pw) (users : User.t_no_pw list) =
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
                ~a:[ a_class [ "dashboard-row"; "h70" ] ]
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
                                  img ~src:"/assets/icons/user.svg" ~alt:""
                                    ~a:[ a_class [ "panel-head-img" ] ]
                                    ();
                                ];
                              div
                                ~a:[ a_class [ "panel-name-wrap" ] ]
                                [
                                  div
                                    ~a:[ a_class [ "panel-name" ] ]
                                    [ txt "Users" ];
                                ];
                              div
                                ~a:[ a_class [ "panel-count-wrap" ] ]
                                [
                                  div
                                    ~a:[ a_class [ "counter1" ] ]
                                    [
                                      txt
                                        (Format.sprintf "%d" (List.length users));
                                    ];
                                ];
                              a
                                ~a:
                                  [
                                    a_class
                                      [ "panel-link-wrap"; "w-inline-block" ];
                                    a_href "/dashboard/users/add";
                                  ]
                                [
                                  div
                                    ~a:[ a_class [ "more-icon" ] ]
                                    [
                                      div ~a:[ a_class [ "plus-v" ] ] [];
                                      div ~a:[ a_class [ "plus-h" ] ] [];
                                    ];
                                ];
                            ];
                          div
                            ~a:[ a_class [ "panel-body" ] ]
                            (match get_user_rows users with
                            | [] ->
                                [
                                  div
                                    ~a:[ a_class [ "w-dyn-list" ] ]
                                    [
                                      div
                                        ~a:[ a_class [ "w-dyn-empty" ] ]
                                        [ div [ txt "No more items found." ] ];
                                    ];
                                ]
                            | rows -> rows);
                        ];
                    ];
                ];
            ];
        ];
    ]

let user request (user : User.t_no_pw) =
  let open Tyxml.Html in
  let permissions_check (perm_label : string) (is_allowed : bool) =
    div
      ~a:[ a_class [ "user-column"; "w-row" ] ]
      [
        div
          ~a:
            [
              a_class
                [
                  "w-col";
                  "w-col-6";
                  "w-col-medium-6";
                  "w-col-small-6";
                  "w-col-tiny-6";
                ];
            ]
          [ div [ txt perm_label ] ];
        div
          ~a:
            [
              a_class
                [
                  "column-2";
                  "w-col";
                  "w-col-6";
                  "w-col-medium-6";
                  "w-col-small-6";
                  "w-col-tiny-6";
                ];
            ]
          [
            img
              ~src:
                (if is_allowed then "/assets/icons/tick.png"
                 else "/assets/icons/xmark.png")
              ~alt:""
              ~a:
                [
                  a_width (if is_allowed then 20 else 15);
                  a_class [ (if is_allowed then "tickmark" else "xmark") ];
                ]
              ();
          ];
      ]
  in

  General.sidebar "USERS" ("Viewing " ^ user.full_name)
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
                                  img ~src:"/assets/icons/user.svg" ~alt:""
                                    ~a:[ a_class [ "panel-head-img" ] ]
                                    ();
                                ];
                              div
                                ~a:[ a_class [ "panel-name-wrap" ] ]
                                [
                                  div
                                    ~a:[ a_class [ "panel-name" ] ]
                                    [ txt user.full_name ];
                                ];
                              a
                                ~a:
                                  [
                                    a_class [ "edit-button"; "w-button" ];
                                    a_href
                                      ("/dashboard/users/"
                                      ^ Base64.encode_string user.email
                                      ^ "/edit");
                                  ]
                                [ txt "EDIT" ];
                              form
                                ~a:
                                  [
                                    a_action
                                      ("/dashboard/users/"
                                      ^ Base64.encode_string user.email
                                      ^ "/remove");
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
                                        a_class [ "edit-button"; "w-button" ];
                                        a_style "padding-bottom: 14px;";
                                      ]
                                    [ txt "REMOVE" ];
                                ];
                            ];
                          div
                            ~a:[ a_class [ "panel-body" ] ]
                            [
                              div
                                ~a:
                                  [
                                    a_id "w-node";
                                    a_class
                                      [
                                        "w-layout-layout";
                                        "quick-stack";
                                        "wf-layout-layout";
                                      ];
                                  ]
                                [
                                  div
                                    ~a:
                                      [
                                        a_id "w-node";
                                        a_class [ "w-layout-cell"; "cell-2" ];
                                      ]
                                    [
                                      h4 [ txt "Details" ];
                                      ul
                                        ~a:
                                          [
                                            a_role [ "list" ];
                                            a_class
                                              [ "list"; "w-list-unstyled" ];
                                          ]
                                        [
                                          li
                                            [
                                              div
                                                ~a:
                                                  [
                                                    a_class
                                                      [ "user-column"; "w-row" ];
                                                  ]
                                                [
                                                  div
                                                    ~a:
                                                      [
                                                        a_class
                                                          [
                                                            "w-col";
                                                            "w-col-6";
                                                            "w-col-medium-6";
                                                            "w-col-small-6";
                                                            "w-col-tiny-6";
                                                          ];
                                                      ]
                                                    [ div [ txt "Name:" ] ];
                                                  div
                                                    ~a:
                                                      [
                                                        a_class
                                                          [
                                                            "column-2";
                                                            "w-col";
                                                            "w-col-6";
                                                            "w-col-medium-6";
                                                            "w-col-small-6";
                                                            "w-col-tiny-6";
                                                          ];
                                                      ]
                                                    [
                                                      div
                                                        ~a:
                                                          [
                                                            a_class
                                                              [ "text-block-5" ];
                                                          ]
                                                        [ txt user.full_name ];
                                                    ];
                                                ];
                                            ];
                                          li
                                            [
                                              div
                                                ~a:
                                                  [
                                                    a_class
                                                      [ "user-column"; "w-row" ];
                                                  ]
                                                [
                                                  div
                                                    ~a:
                                                      [
                                                        a_class
                                                          [
                                                            "w-col";
                                                            "w-col-6";
                                                            "w-col-medium-6";
                                                            "w-col-small-6";
                                                            "w-col-tiny-6";
                                                          ];
                                                      ]
                                                    [ div [ txt "Email:" ] ];
                                                  div
                                                    ~a:
                                                      [
                                                        a_class
                                                          [
                                                            "column-2";
                                                            "w-col";
                                                            "w-col-6";
                                                            "w-col-medium-6";
                                                            "w-col-small-6";
                                                            "w-col-tiny-6";
                                                          ];
                                                      ]
                                                    [
                                                      div
                                                        ~a:
                                                          [
                                                            a_class
                                                              [ "text-block-6" ];
                                                          ]
                                                        [ txt user.email ];
                                                    ];
                                                ];
                                            ];
                                        ];
                                    ];
                                  div
                                    ~a:[ a_class [ "w-layout-cell"; "cell-2" ] ]
                                    [
                                      h4 [ txt "Permissions" ];
                                      ul
                                        ~a:
                                          [
                                            a_role [ "list" ];
                                            a_class
                                              [ "list"; "w-list-unstyled" ];
                                          ]
                                        [
                                          li
                                            [
                                              permissions_check "Read:" true;
                                              permissions_check "Write:"
                                                (user.account_type = `Admin);
                                              permissions_check "Contribute:"
                                                true;
                                              permissions_check "Admin:"
                                                (user.account_type = `Admin);
                                            ];
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

let edit request (user : User.t_no_pw) =
  let open Tyxml.Html in
  let account_type_option value label selected =
    let a =
      if selected then [ a_value value; a_selected () ] else [ a_value value ]
    in
    option ~a (txt label)
  in

  General.sidebar "USERS" ("Editing " ^ user.full_name)
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
                                  img ~src:"/assets/icons/user.svg" ~alt:""
                                    ~a:[ a_class [ "panel-head-img" ] ]
                                    ();
                                ];
                              div
                                ~a:[ a_class [ "panel-name-wrap" ] ]
                                [
                                  div
                                    ~a:[ a_class [ "panel-name" ] ]
                                    [ txt user.full_name ];
                                ];
                              div
                                ~a:[ a_class [ "div-block-2" ] ]
                                [
                                  div
                                    ~a:[ a_class [ "text-block-11" ] ]
                                    [ txt "EDITING" ];
                                ];
                            ];
                          div
                            ~a:[ a_class [ "panel-body" ] ]
                            [
                              div
                                ~a:
                                  [
                                    a_id "w-node";
                                    a_class
                                      [
                                        "w-layout-layout";
                                        "quick-stack";
                                        "wf-layout-layout";
                                      ];
                                  ]
                                [
                                  div
                                    ~a:
                                      [
                                        a_id "w-node";
                                        a_class [ "w-layout-cell"; "cell-2" ];
                                      ]
                                    [
                                      div
                                        ~a:
                                          [
                                            a_class [ "form-block-2"; "w-form" ];
                                          ]
                                        [
                                          form
                                            ~a:
                                              [
                                                a_id "email-form";
                                                a_name "email-form";
                                                a_method `Post;
                                                a_class [ "form-2" ];
                                                a_action
                                                  ("/dashboard/users/"
                                                  ^ Base64.encode_string
                                                      user.email
                                                  ^ "/edit");
                                              ]
                                            [
                                              Unsafe.data
                                                (Dream.csrf_tag request);
                                              h4 [ txt "Details" ];
                                              label
                                                ~a:[ a_label_for "name" ]
                                                [ txt "Name" ];
                                              input
                                                ~a:
                                                  [
                                                    a_input_type `Text;
                                                    a_class
                                                      [
                                                        "user-edit-form-text-field";
                                                        "w-input";
                                                      ];
                                                    a_maxlength 256;
                                                    a_name "name";
                                                    a_value user.full_name;
                                                    a_id "name";
                                                  ]
                                                ();
                                              label
                                                ~a:[ a_label_for "email" ]
                                                [ txt "Email Address" ];
                                              input
                                                ~a:
                                                  [
                                                    a_input_type `Email;
                                                    a_class
                                                      [
                                                        "user-edit-form-text-field";
                                                        "w-input";
                                                      ];
                                                    a_maxlength 256;
                                                    a_name "email";
                                                    a_value user.email;
                                                    a_id "email";
                                                    a_required ();
                                                  ]
                                                ();
                                              h4 [ txt "Permissions" ];
                                              select
                                                ~a:
                                                  [
                                                    a_id "usertype";
                                                    a_name "usertype";
                                                    a_required ();
                                                    a_class
                                                      [
                                                        "select-field";
                                                        "w-select";
                                                      ];
                                                  ]
                                                [
                                                  account_type_option "Admin"
                                                    "Admin"
                                                    (user.account_type = `Admin);
                                                  account_type_option "User"
                                                    "User"
                                                    (user.account_type = `User);
                                                  account_type_option
                                                    "ElevatedUser"
                                                    "Elevated User"
                                                    (user.account_type
                                                   = `ElevatedUser);
                                                ];
                                              input
                                                ~a:
                                                  [
                                                    a_input_type `Submit;
                                                    a_class
                                                      [
                                                        "submit-button";
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
        ];
    ]

let add request =
  let open Tyxml.Html in
  General.sidebar "USERS" "Adding new user"
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
                                  img ~src:"/assets/icons/user.svg" ~alt:""
                                    ~a:[ a_class [ "panel-head-img" ] ]
                                    ();
                                ];
                              div
                                ~a:[ a_class [ "panel-name-wrap" ] ]
                                [
                                  div
                                    ~a:[ a_class [ "panel-name" ] ]
                                    [ txt "ADD USER" ];
                                ];
                            ];
                          div
                            ~a:[ a_class [ "panel-body" ] ]
                            [
                              div
                                ~a:
                                  [
                                    a_id "w-node";
                                    a_class
                                      [
                                        "w-layout-layout";
                                        "quick-stack";
                                        "wf-layout-layout";
                                      ];
                                  ]
                                [
                                  div
                                    ~a:
                                      [
                                        a_id "w-node";
                                        a_class [ "w-layout-cell"; "cell-2" ];
                                      ]
                                    [
                                      div
                                        ~a:
                                          [
                                            a_class [ "form-block-2"; "w-form" ];
                                          ]
                                        [
                                          form
                                            ~a:
                                              [
                                                a_id "email-form";
                                                a_name "email-form";
                                                a_method `Post;
                                                a_class [ "form-2" ];
                                                a_action "/dashboard/users/add";
                                              ]
                                            [
                                              Unsafe.data
                                                (Dream.csrf_tag request);
                                              h4 [ txt "Details" ];
                                              label
                                                ~a:[ a_label_for "name" ]
                                                [ txt "Name" ];
                                              input
                                                ~a:
                                                  [
                                                    a_input_type `Text;
                                                    a_class
                                                      [
                                                        "user-edit-form-text-field";
                                                        "w-input";
                                                      ];
                                                    a_maxlength 256;
                                                    a_name "name";
                                                    a_id "name";
                                                  ]
                                                ();
                                              label
                                                ~a:[ a_label_for "email" ]
                                                [ txt "Email Address" ];
                                              input
                                                ~a:
                                                  [
                                                    a_input_type `Email;
                                                    a_class
                                                      [
                                                        "user-edit-form-text-field";
                                                        "w-input";
                                                      ];
                                                    a_maxlength 256;
                                                    a_name "email";
                                                    a_id "email";
                                                    a_required ();
                                                  ]
                                                ();
                                              label
                                                ~a:[ a_label_for "password" ]
                                                [ txt "Password" ];
                                              input
                                                ~a:
                                                  [
                                                    a_input_type `Password;
                                                    a_class
                                                      [
                                                        "user-edit-form-text-field";
                                                        "w-input";
                                                      ];
                                                    a_maxlength 256;
                                                    a_name "password";
                                                    a_id "password";
                                                    a_required ();
                                                  ]
                                                ();
                                              h4 [ txt "Permissions" ];
                                              select
                                                ~a:
                                                  [
                                                    a_id "usertype";
                                                    a_name "usertype";
                                                    a_required ();
                                                    a_class
                                                      [
                                                        "select-field";
                                                        "w-select";
                                                      ];
                                                  ]
                                                [
                                                  option
                                                    ~a:[ a_value "Admin" ]
                                                    (txt "Admin");
                                                  option
                                                    ~a:[ a_value "User" ]
                                                    (txt "User");
                                                  option
                                                    ~a:
                                                      [ a_value "ElevatedUser" ]
                                                    (txt "Elevated User");
                                                ];
                                              input
                                                ~a:
                                                  [
                                                    a_input_type `Submit;
                                                    a_class
                                                      [
                                                        "submit-button";
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
        ];
    ]
