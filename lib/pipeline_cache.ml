let cache : (unit, Files.Folder.t) Hashtbl.t = Hashtbl.create 1
let set (folder : Files.Folder.t) = Hashtbl.replace cache () folder
let get () = Hashtbl.find_opt cache ()
let mem () = Hashtbl.mem cache ()
