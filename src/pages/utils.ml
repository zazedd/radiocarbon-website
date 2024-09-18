let add_contribution_query id path =
  let query = [ ("contribution", [ id ]) ] in
  Uri.make ~path ~query () |> Uri.to_string

let contrib_query contrib path =
  match contrib with None -> path | Some id -> add_contribution_query id path
