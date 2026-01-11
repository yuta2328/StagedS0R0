(lang dune 3.17)

(name msp_eff)

(generate_opam_files true)

(source
 (github yuta2328/msp_eff))

(authors "Yuta Natsui <yuta.natsui88@gmail.com>")

(using menhir 3.0)

(maintainers "Yuta Natsui <yuta.natsui88@gmail.com>")

(license LICENSE)

(package
 (name msp_eff)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune menhir dolog ppx_deriving_yojson ppx_inline_test qcheck alcotest)
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
