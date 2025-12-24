open Core

type t = {
  values: float array;
  vectors: float array array;
}
[@@deriving sexp]

(**
  Accepts a symmetric matrix and returns its eigenvalues and eigenvectors.
  Note: the eigenvectors are normalized to unit length.
*)
external symm : float array array -> t = "ocaml_gsl_eigen_symmv"

let%expect_test "eigen_1" =
  let m = [| [| 1.0; 0.0; 0.0 |]; [| 0.0; 1.0; 0.0 |]; [| 0.0; 0.0; 1.0 |] |] in
  symm m |> printf !"%{sexp: t}";
  [%expect {|((values (1 1 1)) (vectors ((1 0 0) (0 1 0) (0 0 1))))|}]
