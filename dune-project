(lang dune 3.15)

(name exam)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name exam)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune angstrom)
 (tags
  (topics "to describe" your project))
 (allow_empty))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
