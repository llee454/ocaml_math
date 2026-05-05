open Core

module Params = struct
  type t = {
    lower: float;
    upper: float;
    max_iters: int;
    eps_abs: float;
  } [@@deriving sexp]
end

type t = {
  out: float;
  est_err: float;
  num_iters: int;
} [@@deriving sexp] 

external ocaml_solve_bisection : Params.t -> (float -> float) -> t = "ocaml_solve_bisection"

(**
  Accepts two arguments:
  * params, the search parameters
  * f, a function
  and returns an estimate of the root/zero of the function [f] between
  [params.lower] and [params.upper].
*)
let solve_bisection ~params ~f () = ocaml_solve_bisection params f

let%expect_test "solve_bisection" =
  let res = solve_bisection () ~params:Params.{
      lower = 0.0;
      upper = 2.0;
      max_iters = 1_000;
      eps_abs = 1e-6;
    } ~f:Float.(fun x -> x*x - 1.0)
  in
  printf !"%{sexp: t}\n" res;
  [%expect {|((out 1) (est_err 0) (num_iters 2))|}]
