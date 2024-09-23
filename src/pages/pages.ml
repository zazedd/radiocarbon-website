module Promises = Promises
module Scripts = Scripts

let error ?(msg = "Page not found.") ?(logged_in = true) request =
  [ Error.error ~msg ~logged_in request ]

let mainpage request = [ Mainpage.mainpage request ]
let login request = [ Loginregister.login request ]
let register request = [ Loginregister.register request ]
let dashboard request user = Dashboard.dashboard request user
let users users request user = Users.users request user users
let user user request = Users.user request user
let edit_user user request = Users.edit request user
let add_user = Users.add
let config = Config.config
let add_config = Config.add
let edit_config = Config.edit
let add_folder = Folder.add
let rename_folder = Folder.rename
let file = File.file
let output = Output.output
let add_file = File.add
let contribution = Contribution.contribution
let add_contribution = Contribution.add
let contributions_pending request = Contribution.pending_approval request
let base ttl bdy extra_script = [ General.base ttl bdy extra_script ]
