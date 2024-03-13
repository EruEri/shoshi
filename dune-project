(lang dune 3.11)

(name shoshi)

(using menhir 2.1)

(version "0.1.0")

(generate_opam_files true)

(source
 (github EruEri/shoshi))

(authors "EruEri")

(maintainers "EruEri")

(license GPL-3.0)

(package
 (name shoshi)
 (synopsis "A little bibtex manager")
 (description "A longer description")
 (depends ocaml dune cmdliner menhir)
 (tags
  (cli bibtex)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
