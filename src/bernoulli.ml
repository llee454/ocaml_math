(** This module defines functions for computing the Bernoulli numbers. *)
open Core
open Basic
open Float

(** Returns the nth Bernoulli number (B^+_n). *)
let bernoulli n =
  isum () ~init:0 ~max:n ~f:(fun k ->
    Int.(1//(1 + k)) *
    isum () ~init:0 ~max:k ~f:(fun j ->
      (if Int.(j mod 2 = 0) then 1.0 else -1.0)*
      (float @@ Int.(pow (1 + j) n))*
      (Bigint.to_float @@ binom_coeff () ~n:k ~k:j)
    )
  )

let%expect_test "bernoulli" =
  [0; 1; 2; 3; 4; 5; 6; 7; 8 ]
  |> List.iter ~f:(fun n -> printf "%0.4f " @@ bernoulli n);
  [%expect {| 1.0000 0.5000 0.1667 0.0000 -0.0333 0.0000 0.0238 0.0000 -0.0333 |}]
