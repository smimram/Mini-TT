(lang dune 2.0)
(name minitt)

(generate_opam_files true)

(source (github smimram/Mini-TT))
(authors "Samuel Mimram")
(maintainers "samuel.mimram@polytechnique.edu")

(package
 (name minitt)
 (synopsis "Mini-TT.")
)
(using menhir 2.0)
