open Tyxml.Html
open Website_lib
open Contributions

let contributions ~fn _request (contribs : Contributions.t list)
    (user : User.t_no_pw) =
  let open Tyxml.Html in
  let open Contributions in
  let redir id =
    a
      ~a:
        [
          a_href
            ("/dashboard" |> Utils.add_contribution_query (id |> string_of_int));
        ]
  in
  let contribution (contrib : Contributions.t) =
    tr
      ~a:[ a_class [ "contribution-table" ] ]
      [
        td
          ~a:[ a_class [ "contributions-left" ] ]
          [ redir contrib.id [ txt ("#" ^ string_of_int contrib.id) ] ];
        td
          ~a:[ a_class [ "contributions-middle" ] ]
          [
            txt contrib.title;
            space ();
            space ();
            span
              [
                (match contrib.status with
                | `Unmerged ->
                    img ~alt:"" ~src:"/assets/icons/waiting.svg"
                      ~a:
                        [
                          a_class [ "nav-icon-2"; "black_svg" ];
                          a_title "Not submitted yet!";
                        ]
                      ()
                | `Submitted ->
                    img ~alt:"" ~src:"/assets/icons/pending.svg"
                      ~a:
                        [
                          a_class [ "nav-icon-2" ];
                          a_title "Awaiting for approval!";
                        ]
                      ()
                | `Merged ->
                    img ~alt:"" ~src:"/assets/icons/tick.png"
                      ~a:[ a_class [ "nav-icon-2" ]; a_title "Approved!" ]
                      ()
                | `Rejected ->
                    img ~alt:"" ~src:"/assets/icons/xmark.png"
                      ~a:[ a_class [ "nav-icon-2" ]; a_title "Rejected!" ]
                      ()
                | `Restored -> assert false);
              ];
          ];
        td
          ~a:[ a_class [ "contributions-right" ] ]
          [
            span ~a:[ a_style "color: grey" ] [ txt "By: " ]; txt contrib.email;
          ];
      ]
  in
  let unmerged_sumbitted_from_user = contribs |> fn user in
  if List.length unmerged_sumbitted_from_user = 0 then
    Dashboard.no_more_items_no_btn ()
  else
    div
      [
        table
          ~a:[ a_style "border-collapse: collapse; width: 100%;" ]
          (unmerged_sumbitted_from_user |> List.map contribution);
      ]

let pending_approval _request =
  General.sidebar "PENDING CONTRIBUTIONS" ""
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
                                    [ txt "All Contributions" ];
                                ];
                            ];
                          div
                            ~a:[ a_class [ "card-table" ] ]
                            [ div ~a:[ a_id "content" ] [] ];
                        ];
                    ];
                ];
            ];
        ];
    ]

let contribution contrib request (user : User.t_no_pw) =
  General.sidebar "CONTRIBUTION" ""
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
                            ([
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
                                     [
                                       txt
                                         ("Files in contribution #"
                                        ^ string_of_int contrib.id);
                                     ];
                                   div
                                     ~a:[ a_class [ "panel-filter" ] ]
                                     [ txt "for calibration" ];
                                 ];
                             ]
                            @
                            match contrib.status with
                            | `Unmerged ->
                                [
                                  form
                                    ~a:
                                      [
                                        a_action
                                          ("/dashboard/submit-contrib/"
                                          ^ (contrib.id |> string_of_int));
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
                                            a_id "edit-button";
                                            a_class
                                              [ "edit-button"; "w-button" ];
                                            a_style "padding-bottom: 14px";
                                          ]
                                        [ txt "SUBMIT FOR REVIEW" ];
                                    ];
                                ]
                            | `Submitted when user.account_type = `Admin ->
                                [
                                  form
                                    ~a:
                                      [
                                        a_action
                                          ("/dashboard/accept-contrib/"
                                          ^ (contrib.id |> string_of_int));
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
                                            a_id "edit-button";
                                            a_class
                                              [ "edit-button"; "w-button" ];
                                            a_style
                                              "padding-bottom: 14px; \
                                               background-color: green;";
                                          ]
                                        [ txt "ACCEPT CONTRIBUTION" ];
                                    ];
                                  form
                                    ~a:
                                      [
                                        a_action
                                          ("/dashboard/reject-contrib/"
                                          ^ (contrib.id |> string_of_int));
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
                                            a_id "edit-button";
                                            a_class
                                              [
                                                "delete-button-file"; "w-button";
                                              ];
                                            a_style
                                              "padding-bottom: 14px; \
                                               border-radius: 0px;";
                                          ]
                                        [ txt "REJECT CONTRIBUTION" ];
                                    ];
                                ]
                            | `Submitted when user.account_type <> `Admin ->
                                [
                                  div
                                    ~a:
                                      [
                                        a_class [ "contrib-info-text" ];
                                        a_style "color: orange";
                                      ]
                                    [ txt "Reviewing..." ];
                                ]
                            | `Merged ->
                                [
                                  div
                                    ~a:
                                      [
                                        a_class [ "contrib-info-text" ];
                                        a_style "color: green";
                                      ]
                                    [ txt "Accepted!" ];
                                ]
                            | _ -> []);
                          div
                            ~a:[ a_class [ "card-table" ] ]
                            [ div ~a:[ a_id "content" ] [] ];
                        ];
                    ];
                ];
            ];
        ];
    ]

let add request =
  General.sidebar "CONTRIBUTIONS" "Adding new contribution"
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
                                  img ~src:"/assets/icons/contributions.svg"
                                    ~alt:""
                                    ~a:[ a_class [ "panel-head-img" ] ]
                                    ();
                                ];
                              div
                                ~a:[ a_class [ "panel-name-wrap" ] ]
                                [
                                  div
                                    ~a:[ a_class [ "panel-name" ] ]
                                    [ txt "CONTRIBUTE" ];
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
                                          p
                                            [
                                              txt
                                                "After clicking \"Submit\", \
                                                 you will be redirected to a  \
                                                 page with your own folder \
                                                 inside the database. There, \
                                                 you can upload files, create \
                                                 and configure your own \
                                                 folders (inside the one \
                                                 provided).";
                                            ];
                                          p
                                            [
                                              txt
                                                "Noone can see the files and \
                                                 folders you add, until you \
                                                 submit them.";
                                            ];
                                          p
                                            [
                                              txt
                                                "You will be able to check the \
                                                 outputs that your files \
                                                 produce. When you are happy \
                                                 with them, you can submit \
                                                 your contribution for review.";
                                            ];
                                          p
                                            [
                                              txt
                                                "An administrator can merge \
                                                 your contributions into the \
                                                 main database when you submit \
                                                 them!";
                                            ];
                                          form
                                            ~a:
                                              [
                                                a_id "email-form";
                                                a_name "email-form";
                                                a_method `Post;
                                                a_class [ "form-2" ];
                                                a_action
                                                  "/dashboard/add-contrib";
                                              ]
                                            [
                                              Unsafe.data
                                                (Dream.csrf_tag request);
                                              label
                                                ~a:[ a_label_for "title" ]
                                                [ txt "Title" ];
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
                                                    a_name "title";
                                                    a_id "title";
                                                  ]
                                                ();
                                              label
                                                ~a:[ a_label_for "folername" ]
                                                [ txt "Folder Name" ];
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
                                                    a_name "foldername";
                                                    a_id "foldername";
                                                  ]
                                                ();
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
