(lang dune 3.12)

(name microcaml)

(generate_opam_files true)

(source
 (github francescoborri/microcaml))

(authors "Francesco Borri")

(maintainers "Francesco Borri")

(using menhir 2.1)

(package
 (name microcaml)
 (synopsis "Interpreter for MicrOCaml")
 (description "Interpreter for a educational functional language inspired by OCaml, named MicrOCaml")
 (depends ocaml dune core menhir))

