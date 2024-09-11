open Website_lib
open Tyxml.Html

let line_number i =
  span ~a:[ a_class [ "line-number" ] ] [ i |> string_of_int |> txt ]

let rec indent_from_counter n acc =
  if n = 0 then acc
  else indent_from_counter (n - 1) (space () :: space () :: acc)

let format_script script =
  let lines = String.split_on_char '\n' script in
  let paragraphs =
    List.mapi
      (fun i line ->
        let line_number_span = line_number (i + 1) in
        let _, indentation =
          String.fold_left
            (fun (ended, count) c ->
              if ended then (ended, count)
              else if c = ' ' then (false, count + 1)
              else (true, count))
            (false, 0) line
        in
        let code_content = indent_from_counter indentation [ txt line ] in
        (* Create a flex container for each line *)
        div
          ~a:[ a_class [ "code-line" ] ]
          [ line_number_span; div ~a:[ a_class [ "code-text" ] ] code_content ])
      lines
  in
  div paragraphs

let output_content name path input (config : Files_db.config) script _request =
  [
    div
      ~a:[ a_class [ "card-table" ] ]
      [
        (* Header Section *)
        div
          ~a:[ a_class [ "file-header" ] ]
          [
            div
              ~a:[ a_class [ "card-header-wrap" ] ]
              [
                div
                  ~a:[ a_class [ "header-desc" ] ]
                  [ div ~a:[ a_class [ "header-title" ] ] [ txt name ] ];
              ];
          ];
        (* Card Wrapper Section *)
        div
          ~a:[ a_class [ "card-wrapper" ] ]
          [
            (* Left Side of Card *)
            div
              ~a:[ a_class [ "card-left-wrap" ] ]
              [
                div
                  ~a:[ a_class [ "card-main-info" ] ]
                  [
                    div
                      ~a:[ a_class [ "output-file-desc" ] ]
                      [
                        txt
                          "This output was generated with the following input \
                           CSV and script:";
                      ];
                    div
                      ~a:[ a_class [ "output-dropdowns" ] ]
                      [
                        (* input CSV dropdown *)
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
                                div [ txt "Input CSV" ];
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
                                  ~a:[ a_class [ "inner-dropdown-div" ] ]
                                  [
                                    div
                                      ~a:[ a_class [ "csv-table" ] ]
                                      [ input |> Files.Csv.parse |> File.csv ];
                                  ];
                              ];
                          ];
                        (* script dropdown *)
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
                                div [ txt "Script" ];
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
                                  ~a:[ a_class [ "inner-dropdown-div" ] ]
                                  [
                                    div
                                      ~a:[ a_class [ "code-block" ] ]
                                      [ format_script script ];
                                  ];
                              ];
                          ];
                      ];
                  ];
              ];
            (* right side *)
            div
              ~a:[ a_class [ "card-right-wrap" ] ]
              [
                div
                  ~a:[ a_class [ "card-multi-images" ] ]
                  [
                    div
                      ~a:[ a_class [ "w-layout-layout wf-layout-layout" ] ]
                      [
                        div
                          ~a:[ a_class [ "w-layout-cell" ] ]
                          [
                            a
                              ~a:
                                [
                                  a_class [ "download-button w-button" ];
                                  a_href ("/dashboard/get-output-file/" ^ path);
                                ]
                              [ txt "Download" ];
                            div ~a:[ a_class [ "separator" ] ] [];
                          ];
                        File.output_config ~single:true config;
                      ];
                  ];
              ];
          ];
      ];
  ]

let output _request =
  General.sidebar "OUTPUT" ""
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
