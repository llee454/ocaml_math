open Core

external f : float -> float = "ocaml_gsl_sf_erf"

let%expect_test "Erf.f" =
  printf "%f" (f 2.3);
  [%expect {| 0.998857 |}]

external erf_z : float -> float = "ocaml_gsl_sf_erf_Z"

external erf_q : float -> float = "ocaml_gsl_sf_erf_Q"
