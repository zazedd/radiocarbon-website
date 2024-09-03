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

let sidebar name text =
  let open Tyxml.Html in
  [
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
                  a_href "/dasboard/contributions";
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
