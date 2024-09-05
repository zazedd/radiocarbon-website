let dashboard_files inputs request = Dashboard.dashboard_files request inputs

let config_details config folder request user =
  Config.config_details config folder user request

let config_edit_content folder scripts config request =
  Config.edit_content folder scripts config request

let file_content path file request = File.file_details path file request

let output_file_content name path input config script request =
  Output.output_content name path input config script request
