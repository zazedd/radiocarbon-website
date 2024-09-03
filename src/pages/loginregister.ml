open Tyxml.Html

let login request =
  section
    ~a:[ a_class [ "section-2" ] ]
    [
      div
        ~a:[ a_class [ "container-4" ] ]
        [
          div
            ~a:[ a_class [ "wrapper" ] ]
            [
              div
                ~a:[ a_class [ "content" ] ]
                [
                  div
                    ~a:
                      [
                        a_user_data "ms-form" "login";
                        a_class [ "form-block"; "w-form" ];
                      ]
                    [
                      form
                        ~a:
                          [
                            a_id "login-form";
                            a_name "email-form";
                            a_user_data "name" "Email Form";
                            a_method `Post;
                            a_class [ "form" ];
                            a_action "/login";
                          ]
                        [
                          Unsafe.data (Dream.csrf_tag request);
                          h1
                            ~a:[ a_class [ "heading-2" ] ]
                            [ txt "Welcome back!" ];
                          p
                            ~a:[ a_class [ "pargraph-margin" ] ]
                            [ txt "Please enter your details." ];
                          div
                            ~a:[ a_class [ "field-wrap"; "margin" ] ]
                            [
                              label
                                ~a:
                                  [
                                    a_label_for "email";
                                    a_class [ "field-label" ];
                                  ]
                                [ txt "E-mail" ];
                              input
                                ~a:
                                  [
                                    a_input_type `Email;
                                    a_class [ "field-input"; "w-input" ];
                                    a_maxlength 256;
                                    a_name "email";
                                    a_placeholder "Enter your e-mail";
                                    a_id "email";
                                    a_user_data "ms-member" "email";
                                  ]
                                ();
                            ];
                          div
                            ~a:[ a_class [ "field-wrap" ] ]
                            [
                              label
                                ~a:
                                  [
                                    a_label_for "password";
                                    a_class [ "field-label" ];
                                  ]
                                [ txt "Password" ];
                              input
                                ~a:
                                  [
                                    a_input_type `Password;
                                    a_class [ "field-input"; "w-input" ];
                                    a_maxlength 256;
                                    a_name "password";
                                    a_id "password";
                                    a_user_data "ms-member" "password";
                                  ]
                                ();
                            ];
                          div
                            ~a:[ a_class [ "form-flex" ] ]
                            [
                              a
                                ~a:
                                  [
                                    a_user_data "ms-modal" "forgot-password";
                                    a_href "#";
                                    a_class [ "link" ];
                                  ]
                                [ txt "Forgot your password?" ];
                            ];
                          input
                            ~a:
                              [
                                a_input_type `Submit;
                                a_value "Log in";
                                a_user_data "wait" "Please wait...";
                                a_class [ "button"; "w-button" ];
                              ]
                            ();
                        ];
                    ];
                  div
                    ~a:[ a_class [ "flex-wrap" ] ]
                    [
                      p
                        ~a:[ a_class [ "p-15" ] ]
                        [ txt "Don’t have an account?" ];
                      a
                        ~a:[ a_href "/register"; a_class [ "link" ] ]
                        [ txt "Register here" ];
                    ];
                ];
            ];
        ];
    ]

let register request =
  section
    ~a:[ a_class [ "section-2" ] ]
    [
      div
        ~a:[ a_class [ "container-4" ] ]
        [
          div
            ~a:[ a_class [ "wrapper" ] ]
            [
              div
                ~a:[ a_class [ "content" ] ]
                [
                  div
                    ~a:
                      [
                        a_user_data "ms-form" "login";
                        a_class [ "form-block"; "w-form" ];
                      ]
                    [
                      form
                        ~a:
                          [
                            a_id "register-form";
                            a_name "email-form";
                            a_user_data "name" "Email Form";
                            a_method `Post;
                            a_class [ "form" ];
                            a_action "/register";
                          ]
                        [
                          Unsafe.data (Dream.csrf_tag request);
                          h1 ~a:[ a_class [ "heading-2" ] ] [ txt "Welcome!" ];
                          p
                            ~a:[ a_class [ "pargraph-margin" ] ]
                            [ txt "Please enter your details." ];
                          div
                            ~a:[ a_class [ "field-wrap"; "margin" ] ]
                            [
                              label
                                ~a:
                                  [
                                    a_label_for "name";
                                    a_class [ "field-label" ];
                                  ]
                                [ txt "Name" ];
                              input
                                ~a:
                                  [
                                    a_input_type `Text;
                                    a_class [ "field-input"; "w-input" ];
                                    a_maxlength 256;
                                    a_name "name";
                                    a_placeholder "Enter your name";
                                    a_id "text";
                                    a_user_data "ms-member" "text";
                                  ]
                                ();
                            ];
                          div
                            ~a:[ a_class [ "field-wrap"; "margin" ] ]
                            [
                              label
                                ~a:
                                  [
                                    a_label_for "email";
                                    a_class [ "field-label" ];
                                  ]
                                [ txt "E-mail" ];
                              input
                                ~a:
                                  [
                                    a_input_type `Email;
                                    a_class [ "field-input"; "w-input" ];
                                    a_maxlength 256;
                                    a_name "email";
                                    a_placeholder "Enter your e-mail";
                                    a_id "email";
                                    a_user_data "ms-member" "email";
                                  ]
                                ();
                            ];
                          div
                            ~a:[ a_class [ "field-wrap"; "margin" ] ]
                            [
                              label
                                ~a:
                                  [
                                    a_label_for "password";
                                    a_class [ "field-label" ];
                                  ]
                                [ txt "Password" ];
                              input
                                ~a:
                                  [
                                    a_input_type `Password;
                                    a_class [ "field-input"; "w-input" ];
                                    a_maxlength 256;
                                    a_name "password";
                                    a_placeholder "*******";
                                    a_id "password";
                                    a_user_data "ms-member" "password";
                                  ]
                                ();
                            ];
                          input
                            ~a:
                              [
                                a_input_type `Submit;
                                a_value "Register";
                                a_user_data "wait" "Please wait...";
                                a_class [ "button"; "w-button" ];
                              ]
                            ();
                        ];
                    ];
                ];
            ];
        ];
    ]
