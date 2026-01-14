(lang dune 3.7)

(using menhir 2.1)

(name onoki)

(version 0.1.0)

(generate_opam_files true)

(source
 (github iho/onoki))

(license MIT)

(authors "Ihor Horobets")

(maintainers "Ihor Horobets")

(package
 (name onoki)
 (synopsis "Functional OCaml-like language targeting JVM")
 (description
  "A compiler for a functional ML-like language that generates JVM bytecode, written in OCaml")
 (depends
  (ocaml
   (>= 5.3))
  dune
  menhir
  (ppx_deriving
   (>= 5.0))
  sexplib0))
