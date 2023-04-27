(lang dune 3.7)

(name okotoba)

(generate_opam_files true)

(source
 (github EruEri/okotoba))

(authors "EruEri")

(maintainers "EruEri")

(license GPL-3.0)

(package
 (name okotoba)
 (synopsis "A little program to store translations")
 (depends ocaml dune cmdliner yojson unix)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
