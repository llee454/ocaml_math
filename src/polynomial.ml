open Core

module Quadratic = struct
  type t = {
    n: int;
    x0: float;
    x1: float;
  } [@@deriving sexp]

  (**
    Accepts three parameters: [a], [b], and [c], and returns the real
    roots of the quadratic equation ax^2 + bx + c = 0.
  *)
  external solve : float -> float -> float -> t = "ocaml_gsl_poly_solve_quadratic"

  let%expect_test "solve" =
    solve 1.3 2.7 (-3.0) |> printf !"%{sexp: t}";
    [%expect {| ((n 2) (x0 -2.87859595598549) (x1 0.80167287906241325)) |}]
end

module Cubic = struct
  type t = {
    n: int;
    x0: float;
    x1: float;
    x2: float;
  } [@@deriving sexp]

  (**
    Accepts three parameters: [a], [b], and [c], and returns the real
    roots of the cubic equation x^3 + ax^2 + bx + c = 0.
  *)
  external solve : float -> float -> float -> t = "ocaml_gsl_poly_solve_cubic"

  let%expect_test "solve" =
    solve (-3.0) 1.0 1.2 |> printf !"%{sexp: t}";
    [%expect {|
     ((n 3) (x0 -0.46178620637244294) (x1 1.1005076535251057)
      (x2 2.3612785528473363))
    |}]

end
