(lang dune 3.12)
(name asemio_stats)
(license GPL-3.0)
(authors "Larry D. Lee Jr. <llee454@gmail.com>")
(maintainers "Larry D. Lee Jr. <llee454@gmail.com>")
(source (github llee454/asemio_stats))
(version 1.0.1)
(generate_opam_files)
(package
 (name asemio_stats)
 (synopsis "A collection of mathematical and statistical functions used at Asemio.")
 (description "
  Asemio Stats is a collection of mathematical and statistical
  functions used for statistical research and modeling at Asemio. This
  library largely wraps the Gnu Scientific Library (GSL).
  ")
 (depends
  core
  ppx_jane
  eio_main
  ppx_profile
  conf-gsl
  plplot
 )
)
(use_standard_c_and_cxx_flags false)
