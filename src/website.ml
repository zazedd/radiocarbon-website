open Lwt.Infix

let run handler : ('a, [ `Msg of string ]) result Lwt.t =
  Dream.serve ~port:8081 handler >|= fun () -> Ok ()

let handler =
  Dream.logger @@ Dream.memory_sessions
  @@ Dream.router
       begin
         [
           (* NO LOGIN REQUIRED *)
           Dream.get "/" @@ Handlers.General.Get.mainpage;
           Dream.get "/login" @@ Handlers.General.Get.login;
           Dream.get "/register" @@ Handlers.General.Get.register;
           Dream.post "/login" @@ Handlers.General.Post.login;
           Dream.post "/register" @@ Handlers.General.Post.register;
           Dream.get "/logout" @@ Handlers.General.Post.logout;
           (* ADMIN *)
           Dream.scope "/dashboard"
             [ Handlers.Session.admin_middleware ] (* Users *)
             [
               (* GET *)
               Dream.get "/users" @@ Handlers.Users.Get.users;
               Dream.get "/users/add" @@ Handlers.Users.Get.add;
               Dream.get "/users/:email" @@ Handlers.Users.Get.user;
               Dream.get "/users/:email/edit" @@ Handlers.Users.Get.edit;
               (* POST *)
               Dream.post "/users/add" @@ Handlers.Users.Post.add;
               Dream.post "/users/:email/edit" @@ Handlers.Users.Post.edit;
               Dream.post "/users/:email/remove" @@ Handlers.Users.Post.remove;
             ];
           Dream.scope "/dashboard"
             [ Handlers.Session.admin_middleware ] (* Contributions *)
             [
               (* GET *)
               Dream.get "/contributions"
               @@ Handlers.Contributions.Get.admin_view
                    "/dashboard/get-all-contribs";
             ];
           (* ADMIN OR CONTRIBUTION *)
           Dream.scope "/dashboard"
             [ Handlers.Session.admin_or_contrib_middleware ] (* Config *)
             [
               (* GET *)
               Dream.get "/config/add/**" @@ Handlers.Configs.Get.add;
               Dream.get "/config/edit/**"
               @@ Handlers.Configs.Get.edit "/dashboard/get-config-edit";
               (* PROMISES *)
               Dream.get "/get-config-edit/**" @@ Handlers.Configs.Promises.edit;
               (* POST *)
               Dream.post "/config/add/**" @@ Handlers.Configs.Post.add;
               Dream.post "/config/edit/**" @@ Handlers.Configs.Post.edit;
               Dream.post "/config/remove/**" @@ Handlers.Configs.Post.remove;
             ];
           Dream.scope "/dashboard"
             [ Handlers.Session.admin_or_contrib_middleware ] (* Files *)
             [
               Dream.get "/add-file/**" @@ Handlers.Files.Get.add;
               Dream.post "/add-file/**" @@ Handlers.Files.Post.add;
               Dream.post "/add-file/" @@ Handlers.Files.Post.add;
               Dream.get "/edit-file/**"
               @@ Handlers.Files.Get.edit "/dashboard/get-edit-file";
               Dream.post "/edit-file/**" @@ Handlers.Files.Post.edit;
               Dream.post "/remove-file/**" @@ Handlers.Files.Post.remove;
               Dream.get "/add-folder/**" @@ Handlers.Files.Get.add_folder;
               Dream.post "/add-folder/**" @@ Handlers.Files.Post.add_folder;
               Dream.get "/rename-folder/**" @@ Handlers.Files.Get.rename_folder;
               Dream.post "/rename-folder/**"
               @@ Handlers.Files.Post.rename_folder;
               Dream.post "/remove-folder/**"
               @@ Handlers.Files.Post.remove_folder;
               Dream.get "/get-edit-file/**" @@ Handlers.Files.Promises.edit;
             ];
           (* EVERYONE *)
           Dream.scope "/dashboard"
             [ Handlers.Session.all_middleware ]
             (* PAGES *)
             [
               (* Dashboard*)
               Dream.get ""
               @@ Handlers.General.Get.dashboard "/dashboard/get-files"
                    "/dashboard/get-contribs";
               (* Configs *)
               Dream.get "/configs/**"
               @@ Handlers.Configs.Get.config "/dashboard/get-config";
               (* Files *)
               Dream.get "/inputs/**"
               @@ Handlers.Files.Get.file "/dashboard/get-file";
               (* Outputs *)
               Dream.get "/outputs/**"
               @@ Handlers.Files.Get.output "/dashboard/get-output";
               (* Contributions *)
               Dream.get "/add-contrib" @@ Handlers.Contributions.Get.add;
               Dream.post "/add-contrib" @@ Handlers.Contributions.Post.add;
               Dream.post "/submit-contrib/:id"
               @@ Handlers.Contributions.Post.submit;
               Dream.post "/accept-contrib/:id"
               @@ Handlers.Contributions.Post.accept;
               Dream.post "/reject-contrib/:id"
               @@ Handlers.Contributions.Post.reject;
             ];
           Dream.scope "/dashboard"
             [ Handlers.Session.all_middleware ] (* PROMISES *)
             [
               (* Configs *)
               Dream.get "/get-config/**" @@ Handlers.Configs.Promises.details;
               (* Files *)
               Dream.get "/get-files"
               @@ Handlers.General.Promises.dashboard_files;
               Dream.get "/get-file/**" @@ Handlers.Files.Promises.details;
               (* Outputs *)
               Dream.get "/get-output/**" @@ Handlers.Files.Promises.output;
               Dream.get "/get-output-file/**"
               @@ Handlers.Files.Promises.output_file;
               (* Contributions *)
               Dream.get "/get-contribs"
               @@ Handlers.Contributions.Promises.from_user;
               Dream.get "/get-all-contribs"
               @@ Handlers.Contributions.Promises.all_submitted_merged_rejected;
               (* Status *)
               Dream.get "/get-pipeline-status/topbar"
               @@ Handlers.General.Promises.pipeline_status_topbar;
               Dream.get "/get-pipeline-status/popup"
               @@ Handlers.General.Promises.pipeline_status_popup;
             ];
           (* STATIC *)
           Dream.get "/assets/**" @@ Dream.static "./website/assets";
         ]
       end
