let dashboard_files inputs request = Dashboard.dashboard_files request inputs

let config_details config folder request user =
  Config.config_details config folder user request

let config_edit_content = Config.edit_content
let file_content = File.file_details
let output_file_content = Output.output_content
let pipeline_popup_content = General.pipeline_popup_content
let pipeline_topbar_content = General.pipeline_topbar_content
