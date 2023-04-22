(lang dune 3.7)

(name TheWitnessSolver)

(generate_opam_files true)

(source
 (github Clematrics/TheWitnessSolver))

(authors "Clematrics")

(maintainers "Clematrics")

(package
 (name TheWitnessSolver)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

(using menhir 2.1)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
