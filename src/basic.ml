open Core

let sum ?f = Array.fold ~init:0.0 ~f:(fun sum x -> sum +. Option.value_map f ~default:x ~f:(fun f -> f x))

let sumi ~f = Array.foldi ~init:0.0 ~f:(fun i sum x -> sum +. f i x)

let%expect_test "sum" =
  printf "%.1f" (sum ~f:Fn.id [| 1.0; 3.5; -2.5; 8.2 |]);
  [%expect {|10.2|}]

let lsum = List.fold ~init:0.0 ~f:(fun acc x -> acc +. x)

let%expect_test "lsum" =
  printf "%.1f" (lsum [ 0.0; 1.0; 2.0; 3.0 ]);
  [%expect {|6.0|}]

let lsumf ~f = List.fold ~init:0.0 ~f:(fun acc x -> acc +. f x)

let%expect_test "lsumf" =
  printf "%.1f" (lsumf ~f:Fn.id [ 1.0; 3.5; -2.5; 8.2 ]);
  [%expect {|10.2|}]

external pow_int : float -> int -> float = "ocaml_gsl_pow_int"

let%expect_test "pow_int_1" =
  printf "%.2f" (pow_int 1.1 2);
  [%expect {|1.21|}]

let%expect_test "pow_int_2" =
  printf "%.4f" (pow_int 3.1415 3);
  [%expect {|31.0035|}]

(** Returns x^y *)
let expt (x : float) (y : float) = exp (y *. log x)

let%expect_test "expt" =
  printf "%.2f" (expt 3.0 7.4);
  [%expect {|3393.89|}]

external fact : int -> float = "ocaml_gsl_sf_fact"

let%expect_test "fact_1" =
  printf "%.1f" (fact 3);
  [%expect {|6.0|}]

let%expect_test "fact_2" =
  printf "%.1f" (fact 2);
  [%expect {|2.0|}]

let%expect_test "fact_3" =
  printf "%.1f" (fact 5);
  [%expect {|120.0|}]
