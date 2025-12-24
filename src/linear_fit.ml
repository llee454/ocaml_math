open Core

type t = {
  c0: float;
  c1: float;
}

external ocaml_gsl_fit_linear : float array -> float array -> t = "ocaml_gsl_fit_linear"

let f = ocaml_gsl_fit_linear
