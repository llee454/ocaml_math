(lang dune 3.20)
(name ocaml_math)
(license GPL-3.0-or-later)
(authors "Larry D. Lee Jr. <llee454@gmail.com>")
(maintainers "Larry D. Lee Jr. <llee454@gmail.com>")
(source (github llee454/ocaml_math))
(version 2.4.0)
(generate_opam_files)
(package
 (name ocaml_math)
 (synopsis "A collection of mathematical and statistical functions")
 (description "
  OCaml Math is a collection of mathematical and statistical
  functions used for statistical research and modeling. This
  library largely wraps the Gnu Scientific Library (GSL).
  ")
 (depends
  core
  dune
  ppx_jane
  eio_main
  conf-gsl
  plplot
 )
)
(use_standard_c_and_cxx_flags false)
