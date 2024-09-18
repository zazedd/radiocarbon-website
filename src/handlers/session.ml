open Website_lib

let tyxml_list_to_string html =
  List.map (Format.asprintf "%a" (Tyxml.Html.pp_elt ())) html
  |> String.concat ""

let serve ?(extra_scripts = [ Tyxml.Html.(script (txt "")) ]) title page =
  Dream.html @@ (Pages.base title page extra_scripts |> tyxml_list_to_string)

let grab_session request =
  match Dream.session_field request "user" with
  | None -> Error ("Sorry, you aren't logged in.", false)
  | Some u -> Ok (User.unmarshal u)

let with_user request page =
  match grab_session request with
  | Error (msg, logged_in) -> Pages.error ~msg ~logged_in request
  | Ok user -> page request user

let all_middleware : Dream.handler -> Dream.handler =
 fun handler request ->
  match grab_session request with
  | Error (msg, logged_in) -> Pages.error ~msg ~logged_in request |> serve msg
  | Ok _ -> handler request

let admin_middleware : Dream.handler -> Dream.handler =
 fun handler request ->
  match grab_session request with
  | Error (_, logged_in) ->
      let msg = "Not allowed" in
      Pages.error ~msg ~logged_in request |> serve msg
  | Ok user ->
      if user.account_type = `Admin then handler request
      else
        Pages.error ~msg:"User not allowed" request
        |> serve "Radiocarbon Calibration"

let admin_and_contrib_middleware : Dream.handler -> Dream.handler =
 fun handler request ->
  match Dream.query request "contribution" with
  | Some _ -> handler request
  | None -> (
      match grab_session request with
      | Error (_, logged_in) ->
          let msg = "Not allowed" in
          Pages.error ~msg ~logged_in request |> serve msg
      | Ok user ->
          if user.account_type = `Admin then handler request
          else
            Pages.error ~msg:"User not allowed" request
            |> serve "Radiocarbon Calibration")

let elevated_user_middleware : Dream.handler -> Dream.handler =
 fun handler request ->
  match grab_session request with
  | Error (_, logged_in) ->
      let msg = "Not allowed" in
      Pages.error ~msg ~logged_in request |> serve msg
  | Ok user ->
      if user.account_type = `Admin || user.account_type = `ElevatedUser then
        handler request
      else
        Pages.error ~msg:"User not allowed" request
        |> serve "Radiocarbon Calibration"
