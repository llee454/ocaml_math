open Core

let sum ?f = Array.fold ~init:0.0 ~f:(fun sum x -> sum +. Option.value_map f ~default:x ~f:(fun f -> f x))

let sumi ~f = Array.foldi ~init:0.0 ~f:(fun i sum x -> sum +. f i x)

let%expect_test "sum" =
  printf "%.1f" (sum ~f:Fn.id [| 1.0; 3.5; -2.5; 8.2 |]);
  [%expect {|10.2|}]

let isum ~init ~max ~f () =
  let acc = ref 0.0 in
  for i = init to max do
    acc := !acc +. f i
  done;
  !acc

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

(**
  Accepts two arguments: [upper] and [lower], and returns [upper!/lower!].

  Warning: this function is undefined if [lower > upper] or if [lower < 0].
*)
let fact_down_to ~upper ~lower () =
  let open Bigint in
  let res = ref (Bigint.of_int 1) in
  for i = upper downto Int.(lower + 1) do
    if Int.(i > 0) then res := (!res * Bigint.of_int i)
  done;
  !res

let%expect_test "fact_down_to" =
  [
    (5, 0);
    (5, 2);
    (5, 3)
  ]
  |> List.map ~f:(fun (upper, lower) -> fact_down_to ~upper ~lower ())
  |> printf !"%{sexp: Bigint.t list}";
  [%expect {| (120 60 20) |}]

let binom_coeff ~n ~k () =
  let open Bigint in
  let res = ref (Bigint.of_int 1) in
  for i = 1 to k do
    res := (!res * (Bigint.of_int n + Bigint.of_int 1 - Bigint.of_int i)) / Bigint.of_int i
  done;
  !res

let%expect_test "binom_coeff" =
  binom_coeff ~n:0 ~k:0 ()
  |> Bigint.to_string
  |> printf "%s" ;
  [%expect {| 1 |}]

let%expect_test "binom_coeff" =
  binom_coeff ~n:8 ~k:2 ()
  |> Bigint.to_string
  |> printf "%s" ;
  [%expect {| 28 |}]

let%expect_test "binom_coeff" =
  binom_coeff ~n:15 ~k:7 ()
  |> Bigint.to_string
  |> printf "%s" ;
  [%expect {| 6435 |}]

let%expect_test "binom_coeff" =
  binom_coeff ~n:1_000 ~k:10 ()
  |> Bigint.to_string
  |> printf "%s" ;
  [%expect {| 263409560461970212832400 |}]
