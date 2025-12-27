open Core

module Symmetric = struct
  type t = {
    values: float array;
    vectors: float array array;
  }
  [@@deriving sexp]

  (**
    Accepts a symmetric matrix and returns its eigenvalues and
    eigenvectors. Note: the eigenvectors are normalized to unit length.
  *)
  external get_eigenvectors : float array array -> t = "ocaml_gsl_eigen_symmv"

  let%expect_test "get_eigenvectors" =
    let m = [| [| 1.0; 0.0; 0.0 |]; [| 0.0; 1.0; 0.0 |]; [| 0.0; 0.0; 1.0 |] |] in
    get_eigenvectors m |> printf !"%{sexp: t}";
    [%expect {|((values (1 1 1)) (vectors ((1 0 0) (0 1 0) (0 0 1))))|}]
end

module Nonsymmetric = struct
  type t = {
    values: Complex.Rect.t array;
    vectors: Complex.Rect.t array array;
  }
  [@@deriving sexp]

  (**
    Accepts a symmetric matrix and returns its eigenvalues and
    eigenvectors. Note: the eigenvectors are normalized to unit length.
  *)
  external get_eigenvectors : float array array -> t = "ocaml_gsl_eigen_nonsymmv"

  let%expect_test "get_eigenvectors" =
    let m = [| [| 1.0; 0.0; 0.0 |]; [| 0.0; 1.0; 0.0 |]; [| 0.0; 0.0; 1.0 |] |] in
    get_eigenvectors m |> printf !"%{sexp: t}";
    [%expect {|
      ((values (((real 1) (imag 0)) ((real 1) (imag 0)) ((real 1) (imag 0))))
       (vectors
        ((((real 1) (imag 0)) ((real 0) (imag 0)) ((real 0) (imag 0)))
         (((real 0) (imag 0)) ((real 1) (imag 0)) ((real 0) (imag 0)))
         (((real 0) (imag 0)) ((real 0) (imag 0)) ((real 1) (imag 0))))))
      |}]

  let%expect_test "get_eigenvectors" =
    let m = [| [| 1.0; 2.0 |]; [| 3.0; 4.0 |] |] in
    get_eigenvectors m |> printf !"%{sexp: t}";
    [%expect {|
      ((values
        (((real -0.37228132326901431) (imag 0))
         ((real 5.3722813232690143) (imag 0))))
       (vectors
        ((((real -0.82456484013239384) (imag 0))
          ((real 0.56576746496899233) (imag 0)))
         (((real -0.4159735579192842) (imag 0))
          ((real -0.90937670913212409) (imag 0))))))
      |}]
end
