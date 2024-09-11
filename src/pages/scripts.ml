(* this is what handles server-sent events for updating content that depends
   on Lwt promises.
*)
let loader ?(extra_action = "") path =
  let open Tyxml.Html in
  script
    (Unsafe.data
       (Printf.sprintf
          {|
          function getResolvedLwtPromise () {
            const content = document.getElementById('content');
            content.innerHTML = `<div class='loader-container'><div class='loader'></div></div>`;
            fetch('%s')
              .then(response => response.text())
              .then(data => { 
                content.innerHTML = data;
                %s;
              })
              .catch(error => content.innerHTML = 'Failed to load.')
          };

          window.onLoad = getResolvedLwtPromise ();
        |}
          path extra_action))

let dropdown =
  let open Tyxml.Html in
  (* this allows the dashboard dropdowns to work *)
  script
    (txt
       {|
         function dropdowns() {
            var dropdowns = document.querySelectorAll('.w-dropdown');
            dropdowns.forEach(function(dropdown) {
                var toggle = dropdown.querySelector('.w-dropdown-toggle');
                var list = dropdown.querySelector('.w-dropdown-list');

                function toggleDropdown() {
                    if (event.target.closest('.folder-button')) {
                        return; 
                    }
                    var isOpen = dropdown.classList.contains('w--open');
                    if (isOpen) {
                        closeDropdown();
                    } else {
                        openDropdown();
                    }
                }

                function openDropdown() {
                    dropdown.classList.add('w--open');
                    list.style.display = 'block';
                }

                function closeDropdown() {
                    dropdown.classList.remove('w--open');
                    list.style.display = 'none';
                }
                toggle.addEventListener('click', toggleDropdown);
            });
            document.addEventListener('click', function(event) {
                dropdowns.forEach(function(dropdown) {
                    var toggle = dropdown.querySelector('.w-dropdown-toggle');
                      if (!dropdown.contains(event.target)) {
                        if (!toggle.contains(event.target)) {
                          var isOpen = dropdown.classList.contains('w--open');
                          if (isOpen) {
                              dropdown.classList.remove('w--open');
                              var list = dropdown.querySelector('.w-dropdown-list');
                              list.style.display = 'none';
                          }
                      }
                    }
                });
            });
         };
      |})

(* asks for confirmation upon clicking to delete file/folder *)
let confirm =
  let open Tyxml.Html in
  script
    (Unsafe.data
       {|
          function confirm() {
            document.getElementById("delete-button").addEventListener("click", function(event) {
              if (this.getAttribute("data-confirmed") !== "true") {
                event.preventDefault(); // Prevent form submission
                this.textContent = "ARE YOU SURE?"; // Change button text
                this.setAttribute("data-confirmed", "true"); // Set flag to true
              }
            });
          }
    |})

let pipeline_popup =
  let open Tyxml.Html in
  script
    (Unsafe.data
       {|
          function pipelinePopup() {
            const popup = document.querySelector('.pipeline-status-popup');
            if (popup.style.display === 'block') {
              popup.style.opacity = 0;
              setTimeout(() => {
                popup.style.display = 'none';
              }, 300);
            } else {
              popup.style.display = 'block';
              setTimeout(() => {
                popup.style.opacity = 1;
              }, 10);
            }
          }

          function closePipelinePopup() {
            const popup = document.querySelector('.pipeline-status-popup');
            popup.style.opacity = 0;
            setTimeout(() => {
              popup.style.display = 'none';
            }, 300);
          }

          document.querySelector('.pipeline-status').addEventListener('click', pipelinePopup);
          document.querySelector('.pipeline-status-tr img').addEventListener('click', closePipelinePopup);
      |})

let pipeline_topbar_popup_loader ~topbar_path ~popup_path =
  let open Tyxml.Html in
  script
    (Unsafe.data
       (Printf.sprintf
          {|
          function getPipelineStatusPromise(divname, path) {
              const content = document.getElementById(divname);
              fetch(path)
                .then(response => response.text())
                .then(data => content.innerHTML = data)
                .catch(error => content.innerHTML = 'Failed to load.');
          }

          let p = '%s';
          let t = '%s';

          document.addEventListener('DOMContentLoaded', function () {
            getPipelineStatusPromise('pipeline-topbar-content', p);
            getPipelineStatusPromise('pipeline-popup-content', t);

            setInterval(() => {
              getPipelineStatusPromise('pipeline-topbar-content', p);
              getPipelineStatusPromise('pipeline-popup-content', t);
            }, 5000);
          });
        |}
          topbar_path popup_path))
