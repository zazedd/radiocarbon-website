(lang dune 3.15)

(name website)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name website)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune ppx_yojson_conv)
 (tags
  (topics "to describe" your project)))

(package
 (name website_lib)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune ppx_yojson_conv)
 (tags
  (topics "to describe" your project)))

(package
 (name pipeline_status)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
