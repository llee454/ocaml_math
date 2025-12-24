open Core
open Basic
open Eigen
open Stats

type t = {
  covariance_matrix: float array array;
  components: Eigen.t;
  proportion_var: float array;
}
[@@deriving sexp]

(**
  Compute the eigenvectors and eigenvalues of the covariance matrix of
  the given data. The result is the Principal Component Analysis (PCA)
  of the data.

  Note: each record is repreented by a column in xs. each data factor is represented by a row in xs.
*)
let f (xs : float array array) : t =
  let covariance_matrix = get_covariance_matrix xs in
  let (Eigen.{ values; _ } as components) = Eigen.symm covariance_matrix in
  let total_var = sum values in
  let proportion_var = Array.map values ~f:Float.((fun x -> x / total_var)) in
  { covariance_matrix; components; proportion_var }
