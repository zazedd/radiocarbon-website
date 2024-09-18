let branch = "main"
let inputs = "inputs"
let outputs = "outputs"
let scripts = "scripts"
let config = "config"
let contrib_prefix = "contrib_"

(* Commit author *)
let author = "RadioCarbon-Pipeline <leonardo+bot@tarides.com>"

module Users = struct
  let c = Irmin_git.config ~bare:true "./db/users"
  let info fmt = Irmin_git_unix.info ~author fmt
end

module Pipeline = struct
  let c = Irmin_git.config ~bare:true "./db/pipeline"

  let info fmt =
    let fmt = "[Website (%s)] " ^^ fmt in
    Irmin_git_unix.info ~author fmt
end

module Contributions = struct
  let c = Irmin_git.config ~bare:true "./db/contributions"
  let info fmt = Irmin_git_unix.info ~author fmt
end
