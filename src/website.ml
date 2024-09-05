open Lwt.Infix

let run handler : ('a, [ `Msg of string ]) result Lwt.t =
  Dream.serve ~port:8081 handler >|= fun () -> Ok ()

let handler =
  Dream.logger @@ Dream.memory_sessions
  @@ Dream.router
       [
         Dream.get "/" @@ Handlers.Get.mainpage;
         Dream.get "/login" @@ Handlers.Get.login;
         Dream.get "/register" @@ Handlers.Get.register;
         Dream.post "/login" @@ Handlers.Post.login;
         Dream.post "/register" @@ Handlers.Post.register;
         Dream.get "/logout" @@ Handlers.Post.logout;
         Dream.scope "/dashboard"
           [ Session.admin_middleware ]
           [
             (* Users *)
             (* GET *)
             Dream.get "/users" @@ Handlers.Get.Users.users;
             Dream.get "/users/add" @@ Handlers.Get.Users.add;
             Dream.get "/users/:email" @@ Handlers.Get.Users.user;
             Dream.get "/users/:email/edit" @@ Handlers.Get.Users.edit;
             (* POST *)
             Dream.post "/users/add" @@ Handlers.Post.Users.add;
             Dream.post "/users/:email/edit" @@ Handlers.Post.Users.edit;
             Dream.post "/users/:email/remove" @@ Handlers.Post.Users.remove;
             (* Config *)
             (* GET *)
             Dream.get "/config/add/**" @@ Handlers.Get.Configs.add;
             Dream.get "/config/edit/**"
             @@ Handlers.Get.Configs.edit "/dashboard/get-config-edit";
             Dream.get "/default_config/edit"
             @@ Handlers.Get.Configs.edit_default
                  "/dashboard/get-default-config-edit";
             (* PROMISES *)
             Dream.get "/get-config-edit/**" @@ Handlers.Promises.edit_config;
             Dream.get "/get-default-config-edit"
             @@ Handlers.Promises.edit_default_config;
             (* POST *)
             Dream.post "/config/add/**" @@ Handlers.Post.Configs.add;
             Dream.post "/config/edit/**" @@ Handlers.Post.Configs.edit;
             Dream.post "/config/remove/**" @@ Handlers.Post.Configs.remove;
             Dream.post "/default_config/edit"
             @@ Handlers.Post.Configs.edit_default;
             (* Files *)
             Dream.get "/add-file/**" @@ Handlers.Get.Files.add;
             Dream.post "/add-file/**" @@ Handlers.Post.Files.add;
             Dream.post "/add-file/" @@ Handlers.Post.Files.add;
             Dream.post "/remove-file/**" @@ Handlers.Post.Files.remove;
             Dream.get "/add-folder/**" @@ Handlers.Get.Files.add_folder;
             Dream.post "/add-folder/**" @@ Handlers.Post.Files.add_folder;
             Dream.get "/rename-folder/**" @@ Handlers.Get.Files.rename_folder;
             Dream.post "/rename-folder/**" @@ Handlers.Post.Files.rename_folder;
             Dream.post "/remove-folder/**" @@ Handlers.Post.Files.remove_folder;
           ];
         Dream.scope "/dashboard" [ Session.all_middleware ]
           [
             Dream.get "" @@ Handlers.Get.dashboard "/dashboard/get-files";
             (* Configs *)
             Dream.get "/configs/:path"
             @@ Handlers.Get.Configs.config "/dashboard/get-config";
             Dream.get "/configs/:path/**"
             @@ Handlers.Get.Configs.config "/dashboard/get-config";
             (* PROMISES *)
             Dream.get "/get-config/:path" @@ Handlers.Promises.config_details;
             Dream.get "/get-config/:path/**"
             @@ Handlers.Promises.config_details;
             (* Files *)
             Dream.get "/inputs/:path"
             @@ Handlers.Get.Files.file "/dashboard/get-file";
             Dream.get "/inputs/:path/**"
             @@ Handlers.Get.Files.file "/dashboard/get-file";
             Dream.get "/get-files" @@ Handlers.Promises.dashboard_files;
             Dream.get "/get-file/:path" @@ Handlers.Promises.file_details;
             Dream.get "/get-file/:path/**" @@ Handlers.Promises.file_details;
             (* Outputs *)
             Dream.get "/outputs/:path"
             @@ Handlers.Get.Files.output "/dashboard/get-output";
             Dream.get "/outputs/:path/**"
             @@ Handlers.Get.Files.output "/dashboard/get-output";
             Dream.get "/get-output/:path" @@ Handlers.Promises.output;
             Dream.get "/get-output/:path/**" @@ Handlers.Promises.output;
             Dream.get "/get-output-file/:path" @@ Handlers.Promises.output_file;
             Dream.get "/get-output-file/:path/**"
             @@ Handlers.Promises.output_file;
           ];
         Dream.get "/assets/**" @@ Dream.static "./website/assets";
       ]
