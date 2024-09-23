open Tyxml.Html

let base ttl bdy extra_scripts =
  html
    (head
       (title (txt ttl))
       [
         meta ~a:[ a_charset "utf-8" ] ();
         meta
           ~a:
             [
               a_name "viewport";
               a_content "width=device-width, initial-scale=1";
             ]
           ();
         link ~rel:[ `Stylesheet ] ~href:"/assets/style.css" ();
         link ~rel:[ `Preconnect ] ~href:"https://fonts.googleapis.com" ();
         link ~rel:[ `Preconnect ] ~href:"https://fonts.gstatic.com"
           ~a:[ a_crossorigin `Anonymous ]
           ();
         script
           ~a:
             [
               a_src
                 "https://ajax.googleapis.com/ajax/libs/webfont/1.6.26/webfont.js";
             ]
           (txt "");
         script
           (txt
              "WebFont.load({google: {families: ['Varela:400', 'Varela \
               Round:400', 'PT Sans:400,400italic,700,700italic']}});");
         script
           (txt
              {| 
              ! function(o, c) {
                  var n = c.documentElement,
                      t = ' w-mod-';
                  n.className += t + 'js', ('ontouchstart' in o || o
                      .DocumentTouch && c instanceof\ DocumentTouch) && (n
                      .className += t + 'touch')
              }(window, document);
            |});
         link ~rel:[ `Icon ]
           ~a:[ a_mime_type "image/x-icon" ]
           ~href:"/assets/icons/favicon.ico" ();
         link ~rel:[ `Icon ] ~href:"/assets/icons/favicon.png" ();
       ])
    (body
       begin
         bdy @ extra_scripts
       end)

let format_desc desc =
  let lines = String.split_on_char '\n' desc in
  let paragraphs = List.map (fun line -> p [ txt line ]) lines in
  div paragraphs

let default_status = (`Waiting, "Waiting for pipeline data")

let pipeline_popup_content ~branch (s : (Status.s * string) option) =
  let where =
    if branch = Website_lib.Db_config.branch then " @ Main Database"
    else
      " @ Contribution " ^ (branch |> Website_lib.Contributions.remove_contrib)
  in
  let s = Option.value ~default:default_status s in
  let status_id, desc = s in
  let status, _ = Status.to_strings s in
  let color =
    match status_id with
    | `Success -> "green"
    | `Running -> "orange"
    | `Failed -> "red"
    | `Waiting -> "white"
  in
  let open Tyxml.Html in
  div
    ~a:[ a_class [ "pipeline-popup-inner" ] ]
    [
      div
        ~a:[ a_class [ "w-layout-grid"; "pipeline-status-grid" ] ]
        [
          (* Top-left section *)
          div
            ~a:[ a_class [ "pipeline-popup-tl" ] ]
            [
              div
                ~a:[ a_class [ "pipeline-popup-status-text" ] ]
                [ txt "Status" ];
            ];
          (* Top-right section *)
          div
            ~a:[ a_class [ "pipeline-status-tr" ] ]
            [
              div
                ~a:
                  [
                    a_class [ "pipeline-status-status-text" ];
                    a_style ("color: " ^ color);
                  ]
                [ txt status; span ~a:[ a_style "color: white" ] [ txt where ] ];
            ];
        ];
      (* Popup details section *)
      div
        ~a:[ a_class [ "pipeline-popup-details" ] ]
        [
          h5 ~a:[ a_class [ "heading-4" ] ] [ txt "Details" ];
          div
            ~a:[ a_class [ "pipeline-popup-text-div" ] ]
            [ div ~a:[ a_class [ "text-block-6" ] ] [ format_desc desc ] ];
        ];
    ]

let pipeline_topbar_content (s : (Status.s * string) option) =
  let s = Option.value ~default:default_status s in
  let status_id, _ = s in
  let status, _ = Status.to_strings s in
  let icon =
    "/assets/icons/"
    ^
    match status_id with
    | `Success -> "tick.png"
    | `Running -> "pending.svg"
    | `Failed -> "xmark.png"
    | `Waiting -> "waiting.svg"
  in
  div
    ~a:
      [
        a_class
          [
            "w-layout-layout";
            "quick-stack-2";
            "wf-layout-layout";
            "pipeline-status-top-bar";
          ];
      ]
    [
      (* First cell *)
      div
        ~a:[ a_class [ "w-layout-cell"; "pipeline-status-grid-left" ] ]
        [
          div
            ~a:[ a_class [ "pipeline-status-icon" ] ]
            [
              img ~src:icon ~alt:""
                ~a:[ a_class [ "image-2" ]; a_width 15; a_height 15 ]
                ();
            ];
        ];
      (* Second cell *)
      div
        ~a:[ a_class [ "w-layout-cell" ] ]
        [
          div
            ~a:[ a_class [ "pipeline-status-status" ] ]
            [
              div
                ~a:[ a_class [ "pipeline-status-label" ] ]
                [ txt "Pipeline Status" ];
            ];
        ];
      (* Third cell *)
      div
        ~a:[ a_class [ "w-layout-cell"; "pipeline-status-grid-right" ] ]
        [
          div
            ~a:[ a_class [ "pipeline-status-status" ] ]
            [ div ~a:[ a_class [ "status-text" ] ] [ txt status ] ];
        ];
    ]

let sidebar name text =
  let default_status = Some default_status in
  let open Tyxml.Html in
  [
    div
      ~a:
        [
          a_class [ "pipeline-status-popup" ];
          a_id "pipeline-popup-content";
          a_style "display: none; opacity: 0; transition: opacity 0.3s ease;";
        ]
      [ pipeline_popup_content ~branch:"contrib_0" default_status ];
    div
      ~a:[ a_class [ "ui-top" ] ]
      [
        a
          ~a:[ a_href "/dashboard"; a_class [ "logo-block"; "w-inline-block" ] ]
          [ div ~a:[ a_class [ "logo-icon" ] ] [] ];
        div
          ~a:[ a_class [ "ui-top-wrap" ] ]
          [ h2 ~a:[ a_class [ "ui-title" ] ] [ txt name ] ];
        div
          ~a:[ a_class [ "pipeline-status" ] ]
          [
            div
              ~a:[ a_id "pipeline-topbar-content" ]
              [ pipeline_topbar_content default_status ];
          ];
        div
          ~a:[ a_class [ "ui-top-wrap-2"; "right" ] ]
          [ p ~a:[ a_class [ "ui-text"; "user-text" ] ] [ txt text ] ];
        a
          ~a:
            [
              a_href "/logout";
              a_class [ "w-inline-block"; "right" ];
              a_style "background-color: black; text-decoration: none;";
            ]
          [
            div
              [ p ~a:[ a_class [ "ui-text"; "logout-text" ] ] [ txt "LOGOUT" ] ];
          ];
      ];
    div
      ~a:[ a_class [ "ui-left" ] ]
      [
        div
          ~a:[ a_class [ "ui-left-wrap" ] ]
          [
            a
              ~a:
                [
                  a_href "/dashboard/files";
                  a_class [ "db-nav-item"; "w-inline-block" ];
                ]
              [
                img ~alt:"" ~src:"/assets/icons/files.svg"
                  ~a:[ a_class [ "nav-icon" ] ]
                  ();
              ];
            a
              ~a:
                [
                  a_href "/dashboard/contributions";
                  a_class [ "db-nav-item"; "w-inline-block" ];
                ]
              [
                img ~alt:"" ~src:"/assets/icons/contributions.svg"
                  ~a:[ a_class [ "nav-icon" ] ]
                  ();
              ];
            a
              ~a:
                [
                  a_href "/dashboard/users";
                  a_class [ "db-nav-item"; "w-inline-block" ];
                ]
              [
                img ~alt:"" ~src:"/assets/icons/user.svg"
                  ~a:[ a_class [ "nav-icon" ] ]
                  ();
              ];
          ];
      ];
  ]
