(lang dune 3.20)

(name pvec)

(generate_opam_files true)

(source
 (github nikos-alexandris/pvec))

(authors "Nikos Alexandris <nikos-alexandris@protonmail.com>")

(package
 (name pvec)
 (depends ocaml core_bench hector patricia-tree))
