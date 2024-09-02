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
let add_user request = Users.add request
let config request = Config.config request
let add_config folder scripts request = Config.add folder scripts request
let edit_config request = Config.edit request
let add_folder path request = Folder.add path request
let rename_folder old_name path request = Folder.rename old_name path request
let file request = File.file request
let add_file path request = File.add path request
let base ttl bdy extra_script = [ General.base ttl bdy extra_script ]
