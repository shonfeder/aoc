(lang dune 3.5)

(name aoc)

(generate_opam_files true)

(cram enable)

(source
 (github username/reponame))

(authors "Shon Feder")

(maintainers "Shon Feder")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name aoc)
 (synopsis "AOC")
 (description "AOC AOC AOC")
 (depends ocaml
          dune
          containers
          zlist
          logs
          ppx_inline_test)
 (tags
  ("advent of code" "play")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
