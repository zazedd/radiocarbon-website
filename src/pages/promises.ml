let dashboard_files ?contrib inputs request =
  Dashboard.dashboard_files ~contrib request inputs

let contributions ~fn contribs request user =
  [ Contribution.contributions ~fn request contribs user ]

let config_details ?contrib config folder request user =
  Config.config_details ?contrib config folder user request

let config_edit_content = Config.edit_content
let file_content = File.file_details
let edit_file_content = File.edit_content
let output_file_content = Output.output_content
let pipeline_popup_content = General.pipeline_popup_content
let pipeline_topbar_content = General.pipeline_topbar_content
