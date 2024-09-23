open Website_lib
open Lwt.Infix

let tyxml_list_to_string html =
  List.map (Format.asprintf "%a" (Tyxml.Html.pp_elt ())) html
  |> String.concat ""

let serve ?(extra_scripts = [ Tyxml.Html.(script (txt "")) ]) title page =
  Dream.html @@ (Pages.base title page extra_scripts |> tyxml_list_to_string)

let not_allowed request =
  Pages.error ~msg:"User not allowed" request |> serve "Radiocarbon Calibration"

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
  | Ok user -> (
      match Dream.query request "contribution" with
      | Some id ->
          Format.printf "CONTRIBUTION %s@." id;
          Contributions_db.get id
          >>= begin
                function
                | Ok contrib ->
                    if user.email = contrib.email then handler request
                    else begin
                      match contrib.status with
                      | `Submitted when user.account_type = `Admin ->
                          handler request
                      | `Merged | `Rejected -> handler request
                      | `Unmerged | `Restored | `Submitted ->
                          Format.printf "WHAT@.";
                          not_allowed request
                    end
                | Error msg -> Pages.error ~msg request |> serve msg
              end
      | None -> handler request)

let admin_middleware : Dream.handler -> Dream.handler =
 fun handler request ->
  match grab_session request with
  | Error (_, logged_in) ->
      let msg = "Not allowed" in
      Pages.error ~msg ~logged_in request |> serve msg
  | Ok user ->
      if user.account_type = `Admin then handler request
      else not_allowed request

let admin_or_contrib_middleware : Dream.handler -> Dream.handler =
 fun handler request ->
  match grab_session request with
  | Error (_, logged_in) ->
      let msg = "Not allowed" in
      Pages.error ~msg ~logged_in request |> serve msg
  | Ok user -> begin
      match Dream.query request "contribution" with
      | Some id ->
          Format.printf "CONTRIBUTION %s@." id;
          Contributions_db.get id
          >>= begin
                function
                | Ok contrib ->
                    if user.email = contrib.email then handler request
                    else begin
                      match contrib.status with
                      | `Submitted when user.account_type = `Admin ->
                          handler request
                      | `Merged | `Rejected -> handler request
                      | `Unmerged | `Restored | `Submitted ->
                          Format.printf "WHAT@.";
                          not_allowed request
                    end
                | Error msg -> Pages.error ~msg request |> serve msg
              end
      | None ->
          if user.account_type = `Admin then handler request
          else (
            Format.printf "WHAT v2@.";
            not_allowed request)
    end

let elevated_user_middleware : Dream.handler -> Dream.handler =
 fun handler request ->
  match grab_session request with
  | Error (_, logged_in) ->
      let msg = "Not allowed" in
      Pages.error ~msg ~logged_in request |> serve msg
  | Ok user ->
      if user.account_type = `Admin || user.account_type = `ElevatedUser then
        handler request
      else not_allowed request
