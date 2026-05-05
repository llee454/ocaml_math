open Core

module Params = struct
  type t = {
    lower: float;
    upper: float;
    max_iters: int;
    eps_abs: float;
    eps_rel: float;
  } [@@deriving sexp]
end

type t = {
  out: float;
  est_err: float;
  num_iters: int;
} [@@deriving sexp] 

external ocaml_solve_bisection : Params.t -> (float -> float) -> t = "ocaml_solve_bisection"

external ocaml_solve_falsepos : Params.t -> (float -> float) -> t = "ocaml_solve_falsepos"

external ocaml_solve_brent : Params.t -> (float -> float) -> t = "ocaml_solve_brent"

(**
  Accepts two arguments:
  * params, the search parameters
  * f, a function
  and returns an estimate of the root/zero of the function [f] between
  [params.lower] and [params.upper].

  Note: "if a and b are the endpoints of the interval then f(a) must differ
  in sign from f(b). This ensures that the function crosses zero at least
  once in the interval." (GSL Manual)

  Note: "When there are several roots in the search area, the first root
  to be found will be returned; however it is difficult to predict which of
  the roots this will be." (GSL Manual)

  Note: "It is not possible to use root-bracketing algorithms on
  even-multiplicity roots. For these algorithms the initial interval must
  contain a zero-crossing, where the function is negative at one end of the
  interval and positive at the other end. Roots with even-multiplicity do
  not cross zero, but only touch it instantaneously." (GSL Manual)

  Note: this is the slowest bracketing algorithm in this suite.
*)
let solve_bisection ~params ~f () = ocaml_solve_bisection params f

let%expect_test "solve_bisection" =
  let res = solve_bisection () ~params:Params.{
      lower = 0.0;
      upper = 2.0;
      max_iters = 1_000;
      eps_abs = 1e-6;
      eps_rel = 0.0;
    } ~f:Float.(fun x -> x*x - 1.0)
  in
  printf !"%{sexp: t}\n" res;
  [%expect {|((out 1) (est_err 0) (num_iters 2))|}]

let%expect_test "solve_bisection" =
  let res = solve_bisection () ~params:Params.{
      lower = -10.0;
      upper = 10.0;
      max_iters = 1_000;
      eps_abs = 1e-6;
      eps_rel = 0.0;
    } ~f:Float.(fun x ->
      let () = Gc.full_major () in
      x*x*x - 3.0*x + 7.0)
  in
  printf !"%{sexp: t}\n" res;
  [%expect {|((out -2.4259886145591736) (est_err 5.9604644775390625E-07) (num_iters 26))|}]

(**
  Accepts two arguments:
  * params, the search parameters
  * f, a function
  and returns an estimate of the root/zero of the function [f] between
  [params.lower] and [params.upper].

  Note: "if a and b are the endpoints of the interval then f(a) must differ
  in sign from f(b). This ensures that the function crosses zero at least
  once in the interval." (GSL Manual)

  Note: "When there are several roots in the search area, the first root
  to be found will be returned; however it is difficult to predict which of
  the roots this will be." (GSL Manual)

  Note: "It is not possible to use root-bracketing algorithms on
  even-multiplicity roots. For these algorithms the initial interval must
  contain a zero-crossing, where the function is negative at one end of the
  interval and positive at the other end. Roots with even-multiplicity do
  not cross zero, but only touch it instantaneously." (GSL Manual)

  Note: "The false position algorithm is a method of finding roots based on
  linear interpolation. Its convergence is linear, but it is usually faster
  than bisection." (GSL Manual)
*)
let solve_falsepos ~params ~f () = ocaml_solve_falsepos params f

let%expect_test "solve_falsepos" =
  let res = solve_falsepos () ~params:Params.{
      lower = -10.0;
      upper = 10.0;
      max_iters = 1_000;
      eps_abs = 1e-12;
      eps_rel = 0.0;
    } ~f:Float.(fun x ->
      let () = Gc.full_major () in
      x*x*x - 3.0*x + 7.0)
  in
  printf !"%{sexp: t}\n" res;
  [%expect {| ((out -2.4259887573616221) (est_err 0) (num_iters 13)) |}]

(**
  Accepts two arguments:
  * params, the search parameters
  * f, a function
  and returns an estimate of the root/zero of the function [f] between
  [params.lower] and [params.upper].

  Note: "if a and b are the endpoints of the interval then f(a) must differ
  in sign from f(b). This ensures that the function crosses zero at least
  once in the interval." (GSL Manual)

  Note: "When there are several roots in the search area, the first root
  to be found will be returned; however it is difficult to predict which of
  the roots this will be." (GSL Manual)

  Note: "It is not possible to use root-bracketing algorithms on
  even-multiplicity roots. For these algorithms the initial interval must
  contain a zero-crossing, where the function is negative at one end of the
  interval and positive at the other end. Roots with even-multiplicity do
  not cross zero, but only touch it instantaneously." (GSL Manual)

  Note: "The Brent-Dekker method (referred to here as Brent’s method)
  combines an interpolation strategy with the bisection algorithm. This
  produces a fast algorithm which is still robust." (GSL Manual)
*)
let solve_brent ~params ~f () = ocaml_solve_brent params f

let%expect_test "solve_brent" =
  let res = solve_brent () ~params:Params.{
      lower = -10.0;
      upper = 10.0;
      max_iters = 1_000;
      eps_abs = 1e-12;
      eps_rel = 0.0;
    } ~f:Float.(fun x ->
      let () = Gc.full_major () in
      x*x*x - 3.0*x + 7.0)
  in
  printf !"%{sexp: t}\n" res;
  [%expect {| ((out -2.4259887573616221) (est_err 0) (num_iters 14)) |}]
