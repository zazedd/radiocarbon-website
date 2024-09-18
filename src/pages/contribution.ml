open Tyxml.Html
open Website_lib
open Contributions

let contribution contrib _request =
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
                                    [
                                      txt
                                        ("Files in contribution #"
                                       ^ string_of_int contrib.id);
                                    ];
                                  div
                                    ~a:[ a_class [ "panel-filter" ] ]
                                    [ txt "for calibration" ];
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
                                                "Everyone can see the files \
                                                 and folders you add.";
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
