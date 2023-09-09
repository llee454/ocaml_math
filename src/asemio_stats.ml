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

external gamma : float -> float = "ocaml_gsl_sf_gamma"

type vector = float array [@@deriving compare, equal, sexp]
type matrix = float array array [@@deriving compare, equal, sexp]

let vector_scalar_mult x = Array.map ~f:(( *. ) x)
let vector_add = Array.map2_exn ~f:( +. )
let vector_sub = Array.map2_exn ~f:( -. )

let vector_inner_product (x : float array) (y : float array) =
  Array.fold2_exn x y ~init:0.0 ~f:(fun acc x0 y0 -> acc +. (x0 *. y0))

let%expect_test "vector_inner_product_1" =
  printf "%.2f" (vector_inner_product [| 3.5; 2.0; 1.7 |] [| 4.0; 1.0; 2.8 |]);
  [%expect {|20.76|}]

let%expect_test "vector_inner_product_2" =
  printf "%.1f" (vector_inner_product [| 3.5 |] [| -4.0 |]);
  [%expect {|-14.0|}]

let vector_norm x = sqrt (vector_inner_product x x)

let%expect_test "vector_norm_1" =
  printf "%.1f" (vector_norm [| 1.0; 0.0; 0.0 |]);
  [%expect {|1.0|}]

let%expect_test "vector_norm_2" =
  printf "%.1f" (vector_norm [| -1.0 /. sqrt 2.0; 1.0 /. sqrt 2.0 |]);
  [%expect {|1.0|}]

let%expect_test "vector_norm_3" =
  printf "%.1f" (vector_norm [| -3.0 /. sqrt 2.0; 3.0 /. sqrt 2.0 |]);
  [%expect {|3.0|}]

let vector_matrix_mult (m : float array array) (x : float array) =
  let nrows = Array.length m in
  if [%equal: int] nrows 0 then [||] else Array.init nrows ~f:(fun r -> vector_inner_product m.(r) x)

let%expect_test "vector_matrix_mult" =
  let x = vector_matrix_mult [| [| 3.5; 2.0; 1.7 |]; [| 4.0; 1.0; 2.8 |] |] [| -3.4; 1.8; 5.0 |] in
  printf !"%.2f %.2f" x.(0) x.(1);
  [%expect {|0.20 2.20|}]

(**
  Accepts three arguments: a matrix, a column vector, and a result
  matrix; left multiplies m and x; and writes the result into the
  result matrix.

  Note: this function operates in constant memory and does not make
  any memory allocations.
*)
let vector_matrix_mult_cm (m : float array array) (x : float array) (res : float array) =
  let nrows = Array.length m in
  for i = 0 to nrows - 1 do
    res.(i) <- vector_inner_product m.(i) x
  done

let%expect_test "vector_matrix_mult_cm" =
  let x = Array.create_float_uninitialized ~len:2 in
  vector_matrix_mult_cm [| [| 3.5; 2.0; 1.7 |]; [| 4.0; 1.0; 2.8 |] |] [| -3.4; 1.8; 5.0 |] x;
  printf !"%.2f %.2f" x.(0) x.(1);
  [%expect {|0.20 2.20|}]

(** Accepts two matrices and adds them together. *)
let matrix_add = Array.map2_exn ~f:vector_add

let%expect_test "matrix_add" =
  let m1 = [| [| 1.0; 1.0 |]; [| 3.0; 4.0 |] |] in
  let m2 = [| [| 6.0; 7.0 |]; [| 8.0; 9.0 |] |] in
  matrix_add m1 m2 |> printf !"%{sexp: float array array}";
  [%expect {| ((7 8) (11 13)) |}]

let matrix_sub m0 m1 =
  if Array.length m0 <> Array.length m1
  then failwiths ~here:[%here] "Error: invalid dimensions." () [%sexp_of: unit];
  Array.map2_exn m0 m1 ~f:(Array.map2_exn ~f:( -. ))

let%expect_test "matrix_sub" =
  let m1 = [| [| 1.0; 1.0 |]; [| 3.0; 4.0 |] |] in
  let m2 = [| [| 6.0; 7.0 |]; [| 8.0; 9.0 |] |] in
  matrix_sub m1 m2 |> printf !"%{sexp: float array array}";
  [%expect {| ((-5 -6) (-5 -5)) |}]

let matrix_scalar_mult m x = Array.map m ~f:(Array.map ~f:(( *. ) x))

let%expect_test "matrix_scalar_mult" =
  let m1 = [| [| 1.0; 3.0 |]; [| 5.0; 7.0 |] |] in
  matrix_scalar_mult m1 3.0 |> printf !"%{sexp: float array array}";
  [%expect {| ((3 9) (15 21)) |}]

external matrix_mult : float array array -> float array array -> float array array = "ocaml_matrix_mult"

let%expect_test "matrix_mult" =
  let m1 = [| [| 1.0; 1.0; 2.0 |]; [| 3.0; 4.0; 5.0 |] |] in
  let m2 = [| [| 6.0; 7.0 |]; [| 8.0; 9.0 |]; [| 10.0; 11.0 |] |] in
  matrix_mult m1 m2 |> printf !"%{sexp: float array array}";
  [%expect {|((34 38) (100 112))|}]

let matrix_transpose (xs : float array array) : float array array =
  let nrows = Array.length xs in
  if [%equal: int] nrows 0
  then [||]
  else (
    let ncols = Array.length xs.(0) in
    Array.init ncols ~f:(fun j -> Array.init nrows ~f:(fun i -> xs.(i).(j)))
  )

let%expect_test "matrix_transpose" =
  let xs = [| [| 1.0; 1.0; 2.0 |]; [| 3.0; 4.0; 5.0 |] |] in
  matrix_transpose xs |> printf !"%{sexp: float array array}";
  [%expect {|((1 3) (1 4) (2 5))|}]

(** Accepts a two by two matrix and returns its determinant *)
let matrix_22_det (m : float array array) = Float.((m.(0).(0) * m.(1).(1)) - (m.(0).(1) * m.(1).(0)))

let%expect_test "matrix_22_det" =
  matrix_22_det [| [| 3.0; 7.0 |]; [| 1.0; -4.0 |] |] |> printf !"%{sexp: float}";
  [%expect {| -19 |}]

external matrix_inv : float array array -> float array array = "ocaml_gsl_matrix_inv"

let%expect_test "matrix_inv" =
  let m = [| [| 6.0; 7.0 |]; [| 8.0; 9.0 |] |] in
  matrix_inv m |> printf !"%{sexp: float array array}";
  [%expect {| ((-4.5 3.5) (4 -3)) |}]

(** An optimized function to invert 2x2 matrices *)
let matrix_22_inv m =
  let open Float in
  let det = matrix_22_det m in
  let res = Array.make_matrix ~dimx:2 ~dimy:2 0.0 in
  res.(0).(0) <- m.(1).(1) / det;
  res.(0).(1) <- -m.(0).(1) / det;
  res.(1).(0) <- -m.(1).(0) / det;
  res.(1).(1) <- m.(0).(0) / det;
  res

let%expect_test "matrix_22_inv" =
  let m = [| [| 6.0; 7.0 |]; [| 8.0; 9.0 |] |] in
  matrix_22_inv m |> printf !"%{sexp: float array array}";
  [%expect {| ((-4.5 3.5) (4 -3)) |}]

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
external covariance : xs:float array -> ys:float array -> float = "ocaml_gsl_stats_covariance"

let%expect_test "covariance_1" =
  printf "%.4f"
    (covariance ~xs:[| 0.0; 1.0; 2.0; 3.0; 4.0; 5.0 |]
       ~ys:[| -0.069; 0.0841; 3.3745; 3.9718; 3.6418; 3.9538 |]
    );
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
    cov
  )

module Eigen = struct
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
end

module Pca = struct
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
end

module Linear_fit = struct
  type t = {
    c0: float;
    c1: float;
  }

  external ocaml_gsl_fit_linear : float array -> float array -> t = "ocaml_gsl_fit_linear"

  let f = ocaml_gsl_fit_linear
end

module Erf = struct
  external f : float -> float = "ocaml_gsl_sf_erf_Z"
  external q : float -> float = "ocaml_gsl_sf_erf_Q"
end

module Integrate = struct
  type t = {
    out: float;
    err: float;
    neval: int64;
  }

  external ocaml_integrate : f:(float -> float) -> lower:float -> upper:float -> t = "ocaml_integrate"

  let f = ocaml_integrate

  let%expect_test "Integrate.f_1" =
    let { out; _ } =
      f ~lower:(-1.0) ~upper:1.0 ~f:Float.((fun x -> exp ~-(square x /. 2.0) /. (2.0 *. sqrt_pi)))
    in
    printf "%.4f" out;
    [%expect {|0.4827|}]

  external integration_qag : f:(float -> float) -> lower:float -> upper:float -> t
    = "ocaml_integration_qag"

  let g = integration_qag

  let%expect_test "Integrate.g_1" =
    let { out; _ } =
      g ~lower:(-1.0) ~upper:1.0 ~f:Float.((fun x -> exp ~-(square x /. 2.0) /. (2.0 *. sqrt_pi)))
    in
    printf "%.4f" out;
    [%expect {|0.4827|}]

  external integration_qagi : f:(float -> float) -> t = "ocaml_integration_qagi"

  let h = integration_qagi

  let%expect_test "Integrate.h_1" =
    let { out; _ } = h ~f:Float.((fun x -> 2.0 ** ~-(square x))) in
    printf "%.8f" out;
    [%expect {|2.12893404|}]

  external integration_qagp :
    f:(float -> float) -> lower:float -> upper:float -> singularities:float array -> t
    = "ocaml_integration_qagp"

  let qagp = integration_qagp
end

module Nonlinear_fit = struct
  (*
    represents the parameter values used by a function to process an
    argument value
  *)
  type t = {
    ks: float array;
    (* parameter values *)
    x: float; (* argument value *)
  }
  [@@deriving sexp]

  (*
    Accepts a function, f, that has parameters ks and accepts a float
    and returns a float; and a set of initial parameter values,
    ks_init; and a set of data values, xs; and returns the set of
    parameter values that minimize the error between f_ks (x)

    Example: the following code snippet fits the following linear function to the given dataset:

      f x = k0 + k1 x

    let open Gsl.Nonlinear_fit in
    f
      ~f:Float.((fun { ks = [| k0; k1 |]; x } -> k0 + (k1 * x)))
      ~ks_init:[| 0.0; 0.0 |] ~xs:[| 0.0; 1.0; 2.0; 3.0; 4.0; 5.0 |]
      ~ys:[| 3.1415; 5.8598; 8.5781; 11.296; 14.014; 16.733 |]
    |> function
    | [| k0; k1 |] -> k0, k1
    | _ -> failwith "Gsl.Nonlinear_fit.f returned an array of length != 2"
  *)
  external f : f:(t -> float) -> ks_init:float array -> xs:float array -> ys:float array -> float array
    = "ocaml_gsl_fit_nlinear"
end

module Stats_tests = struct
  exception N_too_small

  (*
    Computes the Jarque-Bera statistic for the given dataset.

    Note: for n >= 2,000 the distribution of the JB statistic
    approximates a X^2 (Chi-squared) distribution with 2 degrees of
    freedom.

    Warning: this function will throw the N_too_small exception if the
    dataset is smaller than 2,000.

    References:
    1. "The Shapiro-Wilk And Related Tests for Normality",
       [https://math.mit.edu/~rmd/465/shapiro.pdf]
  *)
  let jarque_bera ~xs =
    let n =
      let n = Array.length xs in
      if n < 2000 then raise N_too_small;
      float n
    in
    let u = mean xs in
    (* compute the biased sample deviation *)
    let s = sqrt (sum ~f:(fun x -> Float.square (x -. u)) xs /. n) in
    let skew = sum ~f:(fun x -> pow_int ((x -. u) /. s) 3) xs /. n in
    let kurt = (sum ~f:(fun x -> pow_int ((x -. u) /. s) 4) xs /. n) -. 3.0 in
    n /. 6.0 *. (Float.square skew +. (Float.square kurt /. 4.0))

  (**
    Computes an approximation of the r-th largest value in a random
    sample of n values drawn from a Gaussian distribution having
    the given mean and standard deviation sample.

    Note: this function uses an approximation proposed by Blom (1958).

    Warning: for small n, this approximation is rather rough.

    References:
    1. [Reference Table](https://projecteuclid.org/journalArticle/Download?urlId=10.1214%2Faoms%2F1177728266)
    2. [Algorithm AS 177: Expected Normal Order Statistics (Exact and Approximate)](https://www.jstor.org/stable/2347982 )
  *)
  let order_stat_approx ~r ~n ~mean ~std : float =
    let alpha = 0.375 in
    mean
    +. (cdf_gaussian_pinv ~x:((float r -. alpha) /. (float n -. (2.0 *. alpha) +. 1.0)) ~sigma:1.0 *. std)

  let%expect_test "order_stat_1" =
    printf "%.1f" @@ order_stat_approx ~r:20 ~n:20 ~mean:0.0 ~std:1.0;
    [%expect {|1.9|}]

  (**
    Computes an approximation of mean value of the r-th largest
    values in a random sample of n values drawn from a Gaussian
    distribution with mean 0 and standard deviation 1.

    Note: this function uses a numerical approximation to the exact
    formula as given in [1].

    k * 'integrate (
      x/(1 - cdf (x)) * B_pdf (n, r, 1 - cdf (x)) * pdf (x),
      x);

    References:
    1. [Algorithm AS 177: Expected Normal Order Statistics (Exact and Approximate)](https://www.jstor.org/stable/2347982 )
    2. D. Teichroew, et. al. "Tables of Expected Values of Order
       Statistics and Products of Order Statistics for Samples of
       Size Twenty and Less from the Normal Distribution".
       Numerical Analysis Research, University of California,
       Los Angeles. 
       https://projecteuclid.org/journalArticle/Download?urlId=10.1214%2Faoms%2F1177728266
    3. [Exptected Values of Normal Order Statistics](http://faculty.washington.edu/heagerty/Books/Biostatistics/TABLES/NormalOrder.pdf)
  *)
  let order_stat ~r ~n : float =
    let lower, upper = -6.0, 6.0 in
    (* NOTE: the tests fail when these bounds are expanded. *)
    let Integrate.{ out; _ } =
      Integrate.g ~lower ~upper ~f:(fun x ->
          let p = 1.0 -. Erf.q x in
          float r *. x /. p *. pdf_binomial ~k:r ~p ~n *. Erf.f x
      )
    in
    out

  let%expect_test "order_stat_1" =
    printf "%.4f" (order_stat ~r:8 ~n:10);
    [%expect {|0.6561|}]

  let%expect_test "order_stat_2" =
    printf "%.4f" (order_stat ~r:2 ~n:4);
    [%expect {|-0.2970|}]

  (*
    Computes the Shapiro Francia statistic which parameterizes
    the likelihood that the given sample is drawn from a Gaussian
    distribution with mean 0 and standard deviation 1.
  *)
  let shapiro_francia_stat (xs : float array) : float =
    let n = Array.length xs in
    let m = mean xs in
    Array.sort ~len:n xs ~compare:[%compare: float];
    let os = Array.init n ~f:(fun r -> order_stat ~r ~n) in
    sumi xs ~f:(fun r y -> (y -. m) *. os.(r))
    /. Float.sqrt
         (sumi xs ~f:(fun _ y -> Float.square (y -. m)) *. sumi xs ~f:(fun r _ -> Float.square os.(r)))

  (*
  let%expect_test "shapiro_francia_stat_1" =
    printf "%.4f"
      (shapiro_francia_stat
         [|
           0.664220;
           -0.631950;
           -0.448290;
           0.184850;
           -1.40500;
           0.896160;
           -0.598050;
           -0.425810;
           0.504560;
           0.732380;
           1.91910;
           -0.0628270;
           0.451500;
           -0.581380;
           -1.07650;
           -0.245060;
           0.204370;
           -0.646910;
           -0.007770;
           -1.47800;
           -0.573960;
           0.448420;
           -1.25420;
           0.220640;
           -1.18590;
           -1.14360;
           -0.890480;
           -0.90406;
           1.24900;
           -0.875340;
           -0.0253890;
           -0.0810450;
           1.87970;
           0.930510;
           0.865260;
           -0.618640;
           0.110770;
           -2.00530;
           0.328060;
           0.593620;
         |]);
    [%expect {||}]
*)

  (*
    Returns the normalized Shapiro Francia Statistics.

    Note: According to P. Royston "A Toolkit for Testing
    for Non-Normality in Complete and Censored Samples", this
    value is normally distributed with mean:

      mean: ln(ln(n)) - ln(n)
      std: ln(ln(n)) + 2/ln(n)

    where n denotes the sample size.

    This transformation was originally reported by Roysten (1992a)
    for n values from 5 to 5000.

    Warning: this transformation is only valid for n values ranging
    from 5 to 5000.

    References:
    1. ["A Toolkit for Testing for Non-Normality in Complete and Censored Samples"]( https://www.jstor.org/stable/2348109)
  *)

  let shapiro_francia_test (xs : float array) : float =
    let n = float (Array.length xs) in
    let mean = Float.log (Float.log n) -. Float.log n in
    let std = Float.log (Float.log n) +. (2.0 /. Float.log n) in
    let w = Float.log (1.0 -. shapiro_francia_stat xs) in
    let p = cdf_gaussian_p ~x:(w -. mean) ~std in
    let q = 1.0 -. p in
    if Float.(p >= 0.5) then p -. q else q -. p

  (*
  let%expect_test "shapiro_francia_test" =
    printf "%.4f"
      (shapiro_francia_test
         [|
           0.664220;
           -0.631950;
           -0.448290;
           0.184850;
           -1.40500;
           0.896160;
           -0.598050;
           -0.425810;
           0.504560;
           0.732380;
           1.91910;
           -0.0628270;
           0.451500;
           -0.581380;
           -1.07650;
           -0.245060;
           0.204370;
           -0.646910;
           -0.007770;
           -1.47800;
           -0.573960;
           0.448420;
           -1.25420;
           0.220640;
           -1.18590;
           -1.14360;
           -0.890480;
           -0.90406;
           1.24900;
           -0.875340;
           -0.0253890;
           -0.0810450;
           1.87970;
           0.930510;
           0.865260;
           -0.618640;
           0.110770;
           -2.00530;
           0.328060;
           0.593620;
         |]);
    [%expect {||}]
*)

  (*
  let%expect_test "shapiro_francia_test_2" =
    let () = Random.init 5 in
    let xs = Array.init 30 ~f:(fun _ -> Random.float 3.0) in
    printf "%.4f" (shapiro_francia_test xs);
    [%expect {||}]
*)
end

module type Simulated_annealing_arg = sig
  type t

  val copy : t -> t
  val energy : t -> float
  val step : t -> float -> t
  val dist : t -> t -> float
  val print : (t -> unit) option
end

(**
  A demonstration of how to use the simulated annealing function.

  This example fits the following function to the given set of data points:
             1
     f(x) = ----
             x/k
            e
  in this case k should converge toward 2.0 (within a small).

  let open Float in
  let f ~k x = if [%equal: float] k 0.0 then 0.0 else 1.0 / exp (x / k) in
  let data = [| 1.0; 0.6065; 0.3678; 0.2231; 0.1353 |] in
  let { k } =
    let params =
      Gsl.Simulated_annealing.f
        {
          copy = (fun params ->
            let { k } = !params in
            ref { k });
          energy = (fun params ->
            let { k } = !params in
            Gsl.sumi data ~f = (fun i y -> Gsl.pow_int (f ~k (float i) - y) 2));
          step = (fun params delta ->
            let { k } = !params in
            params  = = { k = k + delta });
          dist = (fun params0 params1 ->
            let { k = k0 } = !params0 in
            let { k = k1 } = !params1 in
            Float.abs (k0 - k1));
          print = (fun _ -> ());
          init = (ref { k = 1.0 });
        }
    in
    !params
*)
module Simulated_annealing (M : Simulated_annealing_arg) = struct
  type intf = {
    copy: t -> t;
    energy: t -> float;
    step: t -> float -> t;
    dist: t -> t -> float;
    print: (t -> unit) option;
  }

  and t = {
    intf: intf;
    value: M.t;
  }

  let copy (x : t) : t = { x with value = M.copy x.value }
  let energy (x : t) : float = M.energy x.value
  let step (x : t) (delta : float) : t = { x with value = M.step x.value delta }
  let dist (x : t) (y : t) : float = M.dist x.value y.value
  let print = Option.map M.print ~f:(fun f (x : t) : unit -> f x.value)
  let create_state (value : M.t) : t = { intf = { copy; energy; step; dist; print }; value }

  external simulated_annealing : num_iters:int -> step_size:float -> t -> M.t = "ocaml_siman_solve"

  let f = simulated_annealing
end

module Deriv = struct
  external ocaml_deriv_central : f:(float -> float) -> x:float -> h:float -> float = "ocaml_deriv_central"

  let f = ocaml_deriv_central

  (** Returns the nth derivative of the given function at the given point using an initial step size of h. *)
  let rec nth ~f:(g : float -> float) ~(x : float) ~(h : float) = function
  | n when n <= 1 -> f ~f:g ~x ~h
  | n -> f ~f:(fun x -> nth ~f:g ~x ~h (n - 1)) ~x ~h
end
