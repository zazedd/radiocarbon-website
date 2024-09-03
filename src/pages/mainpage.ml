open Tyxml.Html

let mainpage _request =
  div
    [
      div
        ~a:[ a_class [ "navbar-logo-center" ] ]
        [
          div
            ~a:
              [
                a_user_data "animation" "default";
                a_user_data "collapse" "medium";
                a_user_data "duration" "400";
                a_user_data "easing" "ease";
                a_user_data "easing2" "ease";
                a_role [ "banner" ];
                a_class
                  [ "navbar-logo-center-container"; "shadow-three"; "w-nav" ];
              ]
            [
              div
                ~a:[ a_class [ "container" ] ]
                [
                  div
                    ~a:[ a_class [ "navbar-wrapper-three" ] ]
                    [
                      a
                        ~a:
                          [
                            a_href "#";
                            a_class [ "navbar-brand-three"; "w-nav-brand" ];
                          ]
                        [ (* img ~src:"assets/ephetarides.png" ~alt:"" *)
                          (*   ~a: *)
                          (*     [ *)
                          (*       a_width 240; *)
                          (*       a_img_sizes [ "(max-width: 479px) 80vw, 240px" ]; *)
                          (*       a_srcset *)
                          (*         [ *)
                          (*           `Url_width *)
                          (*             ("assets/ephetarides-p-500.png", 500); *)
                          (*           `Url_width *)
                          (*             ("assets/ephetarides-p-800.png", 800); *)
                          (*           `Url_width ("assets/ephetarides.png", 822); *)
                          (*         ]; *)
                          (*       a_class [ "image" ]; *)
                          (*     ] *)
                          (*   (); *) ];
                      nav
                        ~a:
                          [
                            a_role [ "navigation" ];
                            a_class [ "nav-menu-wrapper-three"; "w-nav-menu" ];
                          ]
                        [
                          div
                            ~a:[ a_class [ "nav-menu-three" ] ]
                            [
                              ul
                                ~a:
                                  [
                                    a_role [ "list" ];
                                    a_class
                                      [ "nav-menu-block"; "w-list-unstyled" ];
                                  ]
                                [
                                  li
                                    [
                                      a
                                        ~a:
                                          [ a_href "#"; a_class [ "nav-link" ] ]
                                        [ txt "New File..." ];
                                    ];
                                  li
                                    [
                                      a
                                        ~a:
                                          [ a_href "#"; a_class [ "nav-link" ] ]
                                        [ txt "Results" ];
                                    ];
                                ];
                              ul
                                ~a:
                                  [
                                    a_role [ "list" ];
                                    a_class
                                      [ "nav-menu-block"; "w-list-unstyled" ];
                                  ]
                                [
                                  li
                                    [
                                      a
                                        ~a:
                                          [
                                            a_href "/register";
                                            a_class [ "nav-link-accent" ];
                                          ]
                                        [ txt "Register" ];
                                    ];
                                  li
                                    ~a:[ a_class [ "mobile-margin-top-10" ] ]
                                    [
                                      a
                                        ~a:
                                          [
                                            a_href "/login";
                                            a_class
                                              [ "button-primary"; "w-button" ];
                                          ]
                                        [ txt "Login" ];
                                    ];
                                ];
                            ];
                        ];
                      div
                        ~a:[ a_class [ "menu-button"; "w-nav-button" ] ]
                        [ div ~a:[ a_class [ "w-icon-nav-menu" ] ] [] ];
                    ];
                ];
            ];
        ];
      section
        ~a:[ a_class [ "section" ] ]
        [
          div
            ~a:
              [
                a_class
                  [ "w-layout-blockcontainer"; "background"; "w-container" ];
              ]
            [
              div
                ~a:
                  [
                    a_class
                      [
                        "w-layout-blockcontainer"; "container-3"; "w-container";
                      ];
                  ]
                [
                  h1
                    ~a:[ a_class [ "heading" ] ]
                    [ txt "Radiocarbon Calibration" ];
                  a
                    ~a:[ a_href "#"; a_class [ "button-primary"; "w-button" ] ]
                    [ txt "get started" ];
                ];
            ];
        ];
    ]
