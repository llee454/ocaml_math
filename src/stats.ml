open Core

external gamma : float -> float = "ocaml_gsl_sf_gamma"

external binomial : n:int -> int -> float = "ocaml_gsl_sf_choose"

let%expect_test "binomial" =
  [(0, 0); (3, 0); (3, 1); (11, 5); (233, 170)]
  |> List.map ~f:(fun (n, k) -> binomial ~n k)
  |> printf !"%{sexp: float list}";
  [%expect {| (1 1 3 462 6.7331706101512676E+57) |}]
  
external mean : float array -> float = "ocaml_mean"

let%expect_test "mean" =
  printf "%.4f" (mean [| 3.1; 2.7; -1.5; 0.5; -3.12 |]);
  [%expect {|0.3360|}]

external sample_variance : float array -> float = "ocaml_gsl_stats_variance"

(* let%expect_test "sample_variance_1" =
   printf "%.4f" (mean [| 0.3786; -1.204; 1.9441; -1.151; -1.184 |]);
   [%expect {|1.9508|}] *)

external pdf_normal : mean:float -> std:float -> float -> float = "ocaml_gsl_ran_gaussian_pdf"

external cdf_gaussian_p : x:float -> std:float -> float = "ocaml_gsl_cdf_gaussian_P"

let%expect_test "cdf_gaussian_p_1" =
  printf "%.4f" (cdf_gaussian_p ~x:0.5 ~std:1.0);
  [%expect {|0.6915|}]

let%expect_test "cdf_gaussian_p_2" =
  printf "%.4f" (cdf_gaussian_p ~x:0.89 ~std:1.0);
  [%expect {|0.8133|}]

let%expect_test "cdf_gaussian_p_3" =
  printf "%.4f" (cdf_gaussian_p ~x:2.14 ~std:3.14);
  [%expect {|0.7522|}]

external cdf_gaussian_q : x:float -> std:float -> float = "ocaml_gsl_cdf_gaussian_Q"

let%expect_test "cdf_gaussian_q_1" =
  printf "%.4f" (1.0 -. cdf_gaussian_q ~x:0.5 ~std:1.0);
  [%expect {|0.6915|}]

let%expect_test "cdf_gaussian_q_2" =
  printf "%.4f" (1.0 -. cdf_gaussian_q ~x:0.89 ~std:1.0);
  [%expect {|0.8133|}]

let%expect_test "cdf_gaussian_q_3" =
  printf "%.4f" (1.0 -. cdf_gaussian_q ~x:2.14 ~std:3.14);
  [%expect {|0.7522|}]

external cdf_chi_squared_p : x:float -> ndf:float -> float = "ocaml_gsl_cdf_chisq_P"

let%expect_test "cdf_chi_squared_p_1" =
  printf "%.4f" (cdf_chi_squared_p ~x:0.0158 ~ndf:1.0);
  [%expect {|0.1000|}]

let%expect_test "cdf_chi_squared_p_2" =
  printf "%.4f" (cdf_chi_squared_p ~x:0.00393 ~ndf:1.0);
  [%expect {|0.0500|}]

let%expect_test "cdf_chi_squared_p_3" =
  printf "%.4f" (cdf_chi_squared_p ~x:0.455 ~ndf:1.0);
  [%expect {|0.5000|}]

let%expect_test "cdf_chi_squared_p_4" =
  printf "%.4f" (cdf_chi_squared_p ~x:3.94 ~ndf:10.0);
  [%expect {|0.0500|}]

external cdf_gaussian_pinv : x:float -> sigma:float -> float = "ocaml_gsl_cdf_gaussian_Pinv"

let%expect_test "cdf_gaussian_pinv_1" =
  printf "%.4f" (cdf_gaussian_pinv ~x:0.5 ~sigma:1.0);
  [%expect {|0.0000|}]

let%expect_test "cdf_gaussian_pinv_2" =
  printf "%.4f" (cdf_gaussian_pinv ~x:0.05 ~sigma:1.0);
  [%expect {|-1.6449|}]

let%expect_test "cdf_gaussian_pinv_2" =
  printf "%.4f" (cdf_gaussian_pinv ~x:0.95 ~sigma:2.0);
  [%expect {|3.2897|}]

external pdf_binomial : k:int -> p:float -> n:int -> float = "ocaml_gsl_ran_binomial_pdf"

let%expect_test "pdf_binomial_1" =
  printf "%.4f" (pdf_binomial ~k:5 ~p:0.5 ~n:10);
  [%expect {|0.2461|}]

external cdf_binomial : k:int -> p:float -> n:int -> float = "ocaml_gsl_cdf_binomial_cdf"

let%expect_test "cdf_binomial_1" =
  printf "%0.4f" (cdf_binomial ~k:5 ~p:0.5 ~n:10);
  [%expect {| 0.6230 |}]

(**
  Accepts a confidence level, alpha, the number of possible instances,
  n, and the probability of each event, p. This function returns the
  the binomial confidence interval.
*)
let binomial_conf_interval ~alpha ~n ~p =
  match p with
  | _ when [%equal: float] p 0.0 -> 0, 0
  | _ when [%equal: float] p 1.0 -> n, n
  | _ ->
    let lower =
      Binary_search.binary_search ()
        ~length:(Fn.const (n + 1))
        ~get:(fun () k -> cdf_binomial ~k ~p ~n)
        ~compare:[%compare: float] `Last_less_than_or_equal_to (alpha /. 2.0)
      |> Option.value ~default:0
    in
    let upper =
      Binary_search.binary_search
        ~pos:(Float.iround_down_exn (float n *. p))
        ()
        ~length:(Fn.const (n + 1))
        ~get:(fun () k -> cdf_binomial ~k ~p ~n)
        ~compare:[%compare: float] `First_greater_than_or_equal_to
        (1.0 -. (alpha /. 2.0))
      |> Option.value_exn ~here:[%here]
    in
    lower, upper

let%expect_test "binomial_conf_interval" =
  [|
    binomial_conf_interval ~alpha:0.05 ~n:10 ~p:0.5;
    binomial_conf_interval ~alpha:0.05 ~n:10 ~p:0.0;
    binomial_conf_interval ~alpha:0.05 ~n:10 ~p:1.0;
    binomial_conf_interval ~alpha:0.05 ~n:3832 ~p:0.0003;
  |]
  |> printf !"%{sexp: (int * int) array}";
  [%expect {| ((1 8) (0 0) (10 10) (0 4)) |}]

external pdf_gamma : a:float -> b:float -> float -> float = "ocaml_gsl_ran_gamma_pdf"

external cdf_beta_p : a:float -> b:float -> float -> float = "ocaml_gsl_cdf_beta_P"

let%expect_test "cdf_beta_p" =
  [|
    cdf_beta_p ~a:3.0 ~b:5.0 0.2;
    cdf_beta_p ~a:7.0 ~b:5.0 0.8
  |]
  |> printf !"%{sexp: float array}";
  [%expect {| (0.14803200000000058 0.9495904256) |}]

external covariance : xs:float array -> ys:float array -> float = "ocaml_gsl_stats_covariance"

let%expect_test "covariance_1" =
  printf "%.4f"
    (covariance ~xs:[| 0.0; 1.0; 2.0; 3.0; 4.0; 5.0 |]
       ~ys:[| -0.069; 0.0841; 3.3745; 3.9718; 3.6418; 3.9538 |] );
  [%expect {|3.1384|}]

external correlation : xs:float array -> ys:float array -> float = "ocaml_gsl_stats_correlation"

(* let%expect_test "correlation_1" =
   printf "%.4f"
     (covariance ~xs:[| 0.0; 1.0; 2.0; 3.0; 4.0; 5.0 |]
        ~ys:[| -1.351; -0.149; 5.299; 2.7622; 3.5947; 3.9727 |]
     );
   [%expect {|0.7348|}] *)

let get_covariance_matrix (xs : float array array) : float array array =
  let n = Array.length xs in
  if [%equal: int] n 0
  then Array.make_matrix ~dimx:0 ~dimy:0 0.0
  else (
    let cov = Array.make_matrix ~dimx:n ~dimy:n 0.0 in
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        cov.(i).(j) <- covariance ~xs:xs.(i) ~ys:xs.(j)
      done
    done;
    cov )
