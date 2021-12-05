(lang dune 2.8)

(name aoc2021)

(generate_opam_files true)

(cram enable)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name aoc2021)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune containers)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
