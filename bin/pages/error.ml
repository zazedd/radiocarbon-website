open Tyxml.Html

let e_msg msg logged_in =
  let err = span ~a:[ a_style "color: red" ] [ "Error: " |> txt ] in
  let msg =
    if not logged_in then
      span [ err; i [ "Sorry, you aren't logged in" |> txt ] ]
    else span [ err; i [ msg |> txt ] ]
  in
  h3 [ msg ]

let error ~msg ~logged_in _request =
  div
    ~a:[ a_class [ "error-container" ] ]
    [
      div ~a:[ a_class [ "error-div" ] ] [ e_msg msg logged_in ];
      div
        ~a:[ a_class [ "error-div" ] ]
        [
          (if logged_in then
             a
               ~a:
                 [
                   a_href "/dashboard";
                   a_class [ "button"; "w-button"; "error-button" ];
                 ]
               [ "Dashboard" |> txt ]
           else
             a
               ~a:
                 [
                   a_href "/login";
                   a_class [ "button"; "w-button"; "error-button" ];
                 ]
               [ "Login" |> txt ]);
        ];
    ]
