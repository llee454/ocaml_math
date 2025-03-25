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

let vector_to_string to_string ?(indent = 0) xs =
  String.make indent ' ' ^ "[" ^ (Array.map xs ~f:to_string |> String.concat_array ~sep:", ") ^ "]\n"

let real_vector_to_string = vector_to_string (sprintf "%0.4f")

let vector_scalar_mult x = Array.map ~f:(( *. ) x)

let vector_add = Array.map2_exn ~f:( +. )

let vector_sub = Array.map2_exn ~f:( -. )

let vector_inner_product = Array.fold2_exn ~init:0.0 ~f:(fun acc x0 y0 -> acc +. (x0 *. y0))

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

let matrix_to_string to_string ?(indent = 0) m =
  let buffer = Buffer.create 100 in
  let tab depth s =
    String.make depth ' ' |> Buffer.add_string buffer;
    Buffer.add_string buffer s
  in
  tab indent "[\n";
  Array.map m ~f:(fun row ->
      String.make (indent + 2) ' '
      ^ "["
      ^ (Array.map row ~f:to_string |> String.concat_array ~sep:", ")
      ^ "]" )
  |> String.concat_array ~sep:",\n"
  |> Buffer.add_string buffer;
  tab indent "\n]\n";
  Buffer.contents buffer

let real_matrix_to_string = matrix_to_string (sprintf "%0.4f")

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
    Array.init ncols ~f:(fun j -> Array.init nrows ~f:(fun i -> xs.(i).(j))) )

let%expect_test "matrix_transpose" =
  let xs = [| [| 1.0; 1.0; 2.0 |]; [| 3.0; 4.0; 5.0 |] |] in
  matrix_transpose xs |> printf !"%{sexp: float array array}";
  [%expect {|((1 3) (1 4) (2 5))|}]

(** Accepts a two by two matrix and returns its determinant *)
let matrix_22_det (m : float array array) = Float.((m.(0).(0) * m.(1).(1)) - (m.(0).(1) * m.(1).(0)))

let%expect_test "matrix_22_det" =
  matrix_22_det [| [| 3.0; 7.0 |]; [| 1.0; -4.0 |] |] |> printf !"%{sexp: float}";
  [%expect {| -19 |}]

external matrix_det : float array array -> float = "ocaml_gsl_matrix_det"

let%expect_test "matrix_det" =
  matrix_det [| [| 3.0; 7.0 |]; [| 1.0; -4.0 |] |] |> printf !"%{sexp: float}";
  [%expect {| -19 |}]

let%expect_test "matrix_det" =
  matrix_det [| [| 1.0; 2.0; 3.0 |]; [| 2.0; 5.0; 6.0 |]; [| 3.0; 6.0; 9.0 |] |]
  |> printf !"%{sexp: float}";
  [%expect {| -0 |}]

let%expect_test "matrix_det" =
  matrix_det [| [| 1.0; 2.0; 3.0 |]; [| -2.0; 5.0; 6.0 |]; [| -3.0; -6.0; 9.0 |] |]
  |> printf !"%{sexp: float}";
  [%expect {| 162 |}]

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

  module QNG_params = struct
    type t = {
      epsabs: float;
      epsrel: float;
    }
    [@@deriving sexp]
  end

  module QAG_params = struct
    type t = {
      epsabs: float;
      epsrel: float;
      limit: int;
    }
    [@@deriving sexp]
  end

  external integration_qng : params:QNG_params.t -> f:(float -> float) -> lower:float -> upper:float -> t
    = "ocaml_integration_qng"

  let qng ?(params : QNG_params.t = QNG_params.{ epsabs = 1e-6; epsrel = 0.0 }) ~f ~lower ~upper () =
    integration_qng ~params ~f ~lower ~upper

  let%expect_test "Integrate.qng" =
    let { out; _ } =
      qng ~lower:(-1.0) ~upper:1.0 ~f:Float.((fun x -> exp ~-(square x /. 2.0) /. (2.0 *. sqrt_pi))) ()
    in
    printf "%.4f" out;
    [%expect {|0.4827|}]

  external integration_qag : params:QAG_params.t -> f:(float -> float) -> lower:float -> upper:float -> t
    = "ocaml_integration_qag"

  let qag ?(params : QAG_params.t = QAG_params.{ epsabs = 1e-6; epsrel = 0.0; limit = 10_000 }) ~f ~lower
    ~upper () =
    integration_qag ~params ~f ~lower ~upper

  let%expect_test "Integrate.gag" =
    let { out; _ } =
      qag ~lower:(-1.0) ~upper:1.0 ~f:Float.((fun x -> exp ~-(square x /. 2.0) /. (2.0 *. sqrt_pi))) ()
    in
    printf "%.4f" out;
    [%expect {|0.4827|}]

  external integration_qagi : params:QAG_params.t -> f:(float -> float) -> t = "ocaml_integration_qagi"

  let qagi ?(params : QAG_params.t = QAG_params.{ epsabs = 1e-6; epsrel = 0.0; limit = 10_000 }) ~f () = integration_qagi ~params ~f

  let%expect_test "Integrate.qagi" =
    let { out; _ } = qagi () ~f:Float.((fun x -> 2.0 ** ~-(square x))) in
    printf "%.8f" out;
    [%expect {| 2.12893307 |}]

  external integration_qagp :
    params:QAG_params.t ->
    f:(float -> float) ->
    lower:float ->
    upper:float ->
    singularities:float array ->
    t = "ocaml_integration_qagp"

  let qagp ?(params : QAG_params.t = QAG_params.{ epsabs = 1e-6; epsrel = 0.0; limit = 10_000 }) ~f ~lower
    ~upper ~singularities () =
    integration_qagp ~params ~f ~lower ~upper ~singularities
end

module Complex = struct
  module Polar = struct
    type t = {
      r: float;
      theta: float;
    }
    [@@deriving fields, sexp]
  end

  module Rect = struct
    type t = {
      real: float;
      imag: float;
    }
    [@@deriving fields, sexp]

    let to_string { real; imag } = sprintf "%0.4f + %0.4fi" real imag

    let zero = { real = 0.0; imag = 0.0 }

    let i = { real = 0.0; imag = 1.0 }

    external ( + ) : t -> t -> t = "ocaml_complex_add"

    let%expect_test "Complex.Rect.add" =
      { real = 2.0; imag = -3.0 } + { real = 1.0; imag = 2.0 } |> printf !"%{sexp: t}";
      [%expect {| ((real 3) (imag -1)) |}]

    external ( - ) : t -> t -> t = "ocaml_complex_sub"

    let%expect_test "Complex.Rect.sub" =
      { real = 2.0; imag = -3.0 } - { real = 1.0; imag = 2.0 } |> printf !"%{sexp: t}";
      [%expect {| ((real 1) (imag -5)) |}]

    external ( * ) : t -> t -> t = "ocaml_complex_mul"

    let%expect_test "Complex.Rect.mul" =
      { real = 2.0; imag = -3.0 } * { real = 1.0; imag = 2.0 } |> printf !"%{sexp: t}";
      [%expect {| ((real 8) (imag 1)) |}]

    let ( +. ) x k = { real = k +. x.real; imag = x.imag }

    let ( -. ) x k = { real = x.real -. k; imag = x.imag }

    let ( *. ) x k = { real = k *. x.real; imag = k *. x.imag }

    external ( / ) : t -> t -> t = "ocaml_complex_div"

    let%expect_test "Complex.Rect.div" =
      { real = 2.0; imag = -3.0 } / { real = 1.0; imag = 2.0 } |> printf !"%{sexp: t}";
      [%expect {| ((real -0.8) (imag -1.4)) |}]

    let ( /. ) x k = { real = x.real /. k; imag = x.imag /. k }

    let mag { real; imag } = Float.(sqrt (square real + square imag))

    let matrix_to_string = matrix_to_string (fun z -> sprintf "(%0.4f, %0.4f)" z.real z.imag)

    let vector_to_string = vector_to_string (fun z -> sprintf "(%0.4f, %0.4f)" z.real z.imag)

    external matrix_inv : t array array -> t array array = "ocaml_gsl_matrix_complex_inv"

    let%expect_test "Complex.Rect.matrix_inv" =
      [|
        [| { real = 1.0; imag = 2.0 }; { real = 3.0; imag = 4.0 } |];
        [| { real = 5.0; imag = 6.0 }; { real = 7.0; imag = 8.0 } |];
      |]
      |> matrix_inv
      |> printf !"%{sexp: t array array}";
      [%expect
        {|
        ((((real -0.49999999999999978) (imag 0.43749999999999978))
          ((real 0.24999999999999989) (imag -0.18749999999999994)))
         (((real 0.37499999999999983) (imag -0.31249999999999983))
          ((real -0.12499999999999993) (imag 0.062499999999999958)))) |}]

    external matrix_det : t array array -> t = "ocaml_gsl_matrix_complex_det"

    let%expect_test "Complex.Rect.matrix_det" =
      [|
        [| { real = 1.0; imag = 1.0 }; { real = 2.0; imag = 2.0 } |];
        [| { real = 3.0; imag = 3.0 }; { real = 4.0; imag = 4.0 } |];
      |]
      |> matrix_det
      |> printf !"%{to_string}";
      [%expect {| 0.0000 + -4.0000i |}]

    let vector_inner_product = Array.fold2_exn ~init:zero ~f:(fun acc x y -> acc + (x * y))

    let vector_matrix_mult (m : t array array) (x : t array) =
      Array.map m ~f:(fun row -> vector_inner_product row x)

    let matrix_mult m0 m1 =
      let n = Array.length m1
      and nrows = Array.length m0 in
      if n = 0 || nrows = 0
      then
        failwiths ~here:[%here]
          "Error: an error occurred while trying to multiply two complex matrices. One or both of these \
           matrices were empty."
          () [%sexp_of: unit];
      if n <> Array.length m0.(0)
      then
        failwiths ~here:[%here]
          "Error: an error occured while trying to multiply two complex matrices. The matrices are not \
           compatible."
          () [%sexp_of: unit];
      let ncols = Array.length m1.(0) in
      let result = Array.make_matrix ~dimx:nrows ~dimy:ncols zero in
      for i = 0 to Int.(nrows - 1) do
        for j = 0 to Int.(ncols - 1) do
          let acc = ref zero in
          for k = 0 to Int.(n - 1) do
            acc := !acc + (m0.(i).(k) * m1.(k).(j))
          done;
          result.(i).(j) <- !acc
        done
      done;
      result

    let%expect_test "matrix_mult" =
      let m1 =
        [|
          [| { real = 1.0; imag = 0.0 }; { real = 2.0; imag = 0.0 } |];
          [| { real = 3.0; imag = 0.0 }; { real = 4.0; imag = 0.0 } |];
        |]
      in
      let m2 =
        [|
          [| { real = 1.0; imag = 0.0 }; { real = 2.0; imag = 0.0 } |];
          [| { real = 3.0; imag = 0.0 }; { real = 4.0; imag = 0.0 } |];
        |]
      in
      matrix_mult m1 m2 |> printf !"%{sexp: t array array}";
      [%expect
        {|
        ((((real 7) (imag 0)) ((real 10) (imag 0)))
         (((real 15) (imag 0)) ((real 22) (imag 0)))) |}]

    let frobenius_norm m = Float.sqrt (sumi m ~f:(fun _i -> sumi ~f:(fun _j z -> Float.square (mag z))))

    let%expect_test "frobenius_norm" =
      frobenius_norm
        [|
          [| { real = 1.0; imag = 0.0 }; { real = -2.0; imag = 0.0 }; { real = 0.0; imag = 0.0 } |];
          [| { real = -1.0; imag = 0.0 }; { real = 3.0; imag = 0.0 }; { real = 2.0; imag = 0.0 } |];
          [| { real = 1.0; imag = 0.0 }; { real = -1.0; imag = 0.0 }; { real = 1.0; imag = 0.0 } |];
        |]
      |> printf "%0.4f";
      [%expect {||}]
  end

  external from_polar : Polar.t -> Rect.t = "ocaml_from_polar"

  let%expect_test "Complex.from_polar" =
    let open Float in
    Polar.[| { r = 2.0; theta = pi }; { r = 1.0; theta = 2.0 * pi } |]
    |> Array.map ~f:from_polar
    |> printf !"%{sexp: Rect.t array}";
    [%expect
      {|
      (((real -2) (imag 2.4492935982947064E-16))
       ((real 1) (imag -2.4492935982947064E-16))) |}]

  module Integrate = struct
    let qag ~lower ~upper ~f =
      Rect.
        {
          real = (Integrate.qag () ~lower ~upper ~f:(fun x -> (f x).real)).out;
          imag = (Integrate.qag () ~lower ~upper ~f:(fun x -> (f x).imag)).out;
        }

    let qagi ~f =
      Rect.
        {
          real = (Integrate.qagi () ~f:(fun x -> (f x).real)).out;
          imag = (Integrate.qagi () ~f:(fun x -> (f x).imag)).out;
        }

    let qagp ~lower ~upper ~singularities ~f =
      Rect.
        {
          real = (Integrate.qagp () ~lower ~upper ~singularities ~f:(fun x -> (f x).real)).out;
          imag = (Integrate.qagp () ~lower ~upper ~singularities ~f:(fun x -> (f x).imag)).out;
        }
  end
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
      Integrate.qag () ~lower ~upper ~f:(fun x ->
          let p = 1.0 -. Erf.q x in
          float r *. x /. p *. pdf_binomial ~k:r ~p ~n *. Erf.f x )
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

  let module SA = Simulated_annealing (struct
    type t = float

    let copy x = x

    let energy x = (x +. 3.0) *. (x +. 7.0)

    let step x dist = x +. Random.float (2.0 *. dist) -. dist

    let dist x y = Float.abs (x -. y)

    let print = None
  end) in
  SA.(f ~num_iters:1_000 ~step_size:1.0 (create_state (-3.0)))
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

module FFT = struct
  external ocaml_fft_real_transform : float array -> float array = "ocaml_fft_real_transform"

  external ocaml_fft_halfcomplex_inverse : float array -> float array = "ocaml_fft_halfcomplex_inverse"

  (**
    Accepts a float array that represents sample output from some
    function f, and returns the fourier coefficients for the discrete
    fourier transform of f.

    Note: this function returns a series of complex values
    (coefficients) in a compact array format. Read the GSL manual for
    details.
  *)
  let to_coeffs = ocaml_fft_real_transform

  let%expect_test "FFT.to_coeffs 1" =
    Array.init 5 ~f:Float.((fun i -> sin (pi *. (i // 5)))) |> to_coeffs |> printf !"%{sexp: float array}";
    [%expect
      {|
      (3.0776835371752536 -1.1755705045849463 1.70845756274088E-16
       -0.36327126400268039 -4.0331212148045958E-17) |}]

  let to_coeffs_slowly data =
    let open Complex.Rect in
    let n = Array.length data in
    Array.init n ~f:(fun j ->
        Array.foldi data ~init:zero ~f:(fun k acc z ->
            acc
            + Complex.from_polar
                Complex.Polar.{ r = z; theta = Float.(-2.0 * pi * of_int j * of_int k / of_int n) } ) )

  let%expect_test "FFT.to_coeffs_slowly" =
    [| 0.0; 1.0; 0.0; 0.0 |] |> to_coeffs_slowly |> Complex.Rect.vector_to_string |> print_endline;
    [| 1.0; 1.0; 0.0; 0.0 |] |> to_coeffs_slowly |> Complex.Rect.vector_to_string |> print_endline;
    [%expect
      {|
      [(1.0000, 0.0000), (0.0000, -1.0000), (-1.0000, -0.0000), (-0.0000, 1.0000)]

      [(2.0000, 0.0000), (1.0000, -1.0000), (0.0000, -0.0000), (1.0000, 1.0000)] |}]

  external ocaml_fft_halfcomplex_unpack : float array -> Complex.Rect.t array
    = "ocaml_fft_halfcomplex_unpack"

  let unpack_coeffs = ocaml_fft_halfcomplex_unpack

  let%expect_test "unpack_coeffs" =
    Array.init 6 ~f:Float.((fun i -> sin (pi *. (i // 5))))
    |> to_coeffs
    |> unpack_coeffs
    |> printf !"%{sexp: Complex.Rect.t array}";
    [%expect
      {|
      (((real 3.077683537175254) (imag 0))
       ((real -1.4265847744427305) (imag -0.82363910354633174))
       ((real -0.11225699414489643) (imag -0.19443481736392254))
       ((real -0.11225699414489643) (imag 0.19443481736392254))
       ((real -1.4265847744427305) (imag 0.82363910354633174))) |}]

  (**
    Accepts a float array that represent discrete fourier series
    coefficients of a function f and returns an approximation of the
    data that was used to generate these coefficients.

    Note: this function accepts a series of complex values (fourier
    coefficients) in a compact array format. Read the GSL manual for
    details.
  *)
  let to_data = ocaml_fft_halfcomplex_inverse

  let%expect_test "FFT.f" =
    let xs = Array.init 10 ~f:Float.((fun i -> sin (pi *. (i // 5)))) in
    let cs = to_coeffs xs in
    printf
      !"original data = %{sexp: float array}\n\
        coeffs = %{sexp: float array}\n\
        approx data = %{sexp: float array}"
      xs cs (to_data cs);
    [%expect
      {|
      original data = (0 0.58778525229247314 0.95105651629515353 0.95105651629515364
       0.58778525229247325 1.2246467991473532E-16 -0.587785252292473
       -0.95105651629515353 -0.95105651629515364 -0.58778525229247325)
      coeffs = (2.2204460492503131E-16 -6.58034846959237E-16 -5 -5.2742482544217396E-17
       -6.8615556431105829E-17 3.6961145430005056E-16 -6.8615556431105829E-17
       2.3014357274088822E-16 -4.4408920985006262E-16 0)
      approx data = (0 0.58778525229247325 0.95105651629515331 0.95105651629515386
       0.58778525229247325 1.1536935706367458E-16 -0.587785252292473
       -0.95105651629515331 -0.95105651629515353 -0.58778525229247336) |}]

  let%expect_test "FFT.f 2" =
    let xs = Array.init 11 ~f:(fun i -> pdf_normal ~mean:0.0 ~std:1.0 (float (i - 5))) in
    let cs = to_coeffs xs in
    printf
      !"original data = %{sexp: float array}\n\
        coeffs = %{sexp: float array}\n\
        approx data = %{sexp: float array}"
      xs cs (to_data cs);
    [%expect
      {|
      original data = (1.4867195147342977E-06 0.00013383022576488537 0.0044318484119380075
       0.053990966513188063 0.24197072451914337 0.3989422804014327
       0.24197072451914337 0.053990966513188063 0.0044318484119380075
       0.00013383022576488537 1.4867195147342977E-06)
      coeffs = (0.99999999318053079 -0.815067674557679 -0.23932546278780145
       0.438062124759543 0.28152543315840334 -0.15085873567432515
       -0.17410026187799929 0.030683229869758123 0.06718690162077201
       -0.0028107640302309574 -0.0195492932356621)
      approx data = (1.4867195147930333E-06 0.00013383022576492272 0.004431848411937985
       0.053990966513188042 0.24197072451914337 0.39894228040143265
       0.24197072451914339 0.053990966513188084 0.00443184841193801
       0.00013383022576488802 1.486719514761966E-06) |}]

  let to_data_slowly coeffs =
    let open Complex.Rect in
    let n = Array.length coeffs in
    Array.init n ~f:(fun j ->
        Array.foldi coeffs ~init:zero ~f:(fun k acc x ->
            let freq =
              Complex.from_polar
                Complex.Polar.{ r = 1.0; theta = Float.(2.0 * pi * of_int j * of_int k / of_int n) }
            in
            acc + (x * freq) )
        /. float n )

  let approx_slowly ~lower ~upper coeffs x =
    let open Complex.Rect in
    let n = Array.length coeffs in
    let j : float = Float.(of_int n * (x - lower) / (upper - lower)) in
    Array.foldi coeffs ~init:zero ~f:(fun k acc x ->
        let freq =
          Complex.from_polar Complex.Polar.{ r = 1.0; theta = Float.(2.0 * pi * j * of_int k / of_int n) }
        in
        acc + (x * freq) )
    /. float n
end

(**
  WARNING: THIS MODULE IS UNSTABLE AND IS NOT FINISHED DO NOT USE!!

  This module defines functions that can be used to mask datasets by
  adding gaussian noise and to analyze these datasets by removing
  gaussian noise.
*)
module Perturb = struct
  (**
    Returns a random number from a normal distribution with mean 0
    and std 1 using the Box Meuller algorithm.
  *)
  let rand_normal () =
    let open Float in
    sqrt (-2.0 * log (Random.float 1.0)) * sin (2.0 * pi * Random.float 1.0)

  let get_sample ~lower ~upper ~num_pts f =
    let delta = Float.((upper - lower) /. of_int Int.(num_pts - 1)) in
    let to_x i = Float.((delta *. of_int i) +. lower) in
    Array.init num_pts ~f:(fun i ->
        let x = to_x i in
        [| x; f x |] )

  let%expect_test "get_sample" =
    [| get_sample ~lower:(-1.0) ~upper:1.0 ~num_pts:4 Fn.id |]
    |> Array.iter ~f:(printf !"%{real_matrix_to_string}");
    [%expect
      {|
      [
        [-1.0000, -1.0000],
        [-0.3333, -0.3333],
        [0.3333, 0.3333],
        [1.0000, 1.0000]
      ] |}]

  let get_sample_data ~lower ~upper ~num_pts f =
    get_sample ~lower ~upper ~num_pts f |> Array.map ~f:(fun x -> x.(1))

  (**
    Accepts a dataset and returns the (gaussian) kernel density
    estimator to approximate the probability density function for xs.
    This is a function that accepts a point and returns an estimate of
    the probability function at that point.

    Note: this function accepts an optional bandwidth parameter tha
    can be used to tune the degree to which the returned pdf function
    under-over smooths the density estimate.

    WARNING: TODO: this function is not converging as expected for normally distributed values.
  *)
  let get_epdf ?(bandwidth = 1.0) xs =
    let open Float in
    let n = Array.length xs in
    (fun x0 -> sum xs ~f:(pdf_normal ~mean:x0 ~std:bandwidth) / (bandwidth * float n))

  let%expect_test "get_epdf" =
    pdf_normal ~mean:0.0 ~std:1.0
    |> get_sample ~lower:(-4.0) ~upper:4.0 ~num_pts:8
    |> printf !"%{real_matrix_to_string}";
    Array.init 1_000 ~f:(fun _ -> rand_normal ())
    |> get_epdf ~bandwidth:0.79
    |> get_sample ~lower:(-4.0) ~upper:4.0 ~num_pts:8
    |> printf !"%{real_matrix_to_string}";
    [%expect
      {|
      [
        [-4.0000, 0.0001],
        [-2.8571, 0.0067],
        [-1.7143, 0.0918],
        [-0.5714, 0.3388],
        [0.5714, 0.3388],
        [1.7143, 0.0918],
        [2.8571, 0.0067],
        [4.0000, 0.0001]
      ]
      [
        [-4.0000, 0.0027],
        [-2.8571, 0.0306],
        [-1.7143, 0.1646],
        [-0.5714, 0.3607],
        [0.5714, 0.3594],
        [1.7143, 0.1584],
        [2.8571, 0.0292],
        [4.0000, 0.0020]
      ]
   |}]

  (**
    Accepts four arguments:
    * lower - the lower bound of the perturbed dataset's range
    * upper - the upper bound of the perturbed dataset's range
    * num_pts - the number of sample points
    * xs - the perturbed dataset
    and returns a sample of num_pts of the perturbed dataset's
    range.
  *)
  let get_epdf_sample ?bandwidth xs = get_sample (get_epdf ?bandwidth xs)

  (**
    Accepts a sample of points from the perturbed dataset's empirical
    cumulative density function and returns the discrete fourier
    transform coefficients.
  *)
  let get_perturbed_dataset_coeffs = FFT.to_coeffs_slowly

  let get_phi_matrix ~num_pts ~lower ~upper ~std =
    let delta = (upper -. lower) /. float (num_pts - 1) in
    let to_x i = (delta *. float i) +. lower in
    let phi k j =
      let acc = ref Complex.Rect.zero in
      for n = 0 to num_pts - 1 do
        for n0 = 0 to num_pts - 1 do
          let open Complex.Rect in
          let w =
            Complex.Polar.{ r = 1.0; theta = Float.(2.0 * pi * Int.(((j * n) - (k * n0)) // num_pts)) }
            |> Complex.from_polar
          in
          acc := !acc + (w *. pdf_normal ~mean:(to_x n) ~std (to_x n0) *. delta /. float num_pts)
        done
      done;
      !acc
    in
    Array.init num_pts ~f:(fun k -> Array.init num_pts ~f:(phi k))

  let%expect_test "get_phi_matrix" =
    let open Float in
    let num_pts = 35 (* was 50 *)
    and lower = -20.0 (* TODO: should be (at least) -20 to 20 was -10 to 10 *)
    and upper = 20.0
    and std = 3.0 in
    let unperturbed_dataset =
      get_sample_data ~lower ~upper ~num_pts (fun x -> pdf_normal ~mean:0.0 ~std:5.0 x)
    in
    let perturbed_dataset =
      get_sample_data ~lower ~upper ~num_pts (fun x0 ->
          (Integrate.qagi () ~f:(fun x ->
               pdf_normal ~mean:0.0 ~std:5.0 x * pdf_normal ~mean:x ~std x0 ) )
            .out )
    in
    let phi_matrix = get_phi_matrix ~num_pts ~lower ~upper ~std in
    let phi_matrix_inv = Complex.Rect.matrix_inv phi_matrix in
    let unperturbed_dataset_coeffs = FFT.to_coeffs_slowly unperturbed_dataset in
    let perturbed_dataset_coeffs = FFT.to_coeffs_slowly perturbed_dataset in
    let cs_approx = Complex.Rect.vector_matrix_mult phi_matrix_inv perturbed_dataset_coeffs in
    printf !"determinant: %{Complex.Rect}\n"
      (Complex.Rect.matrix_det (Complex.Rect.matrix_mult phi_matrix phi_matrix_inv));
    printf !"%{Complex.Rect.vector_to_string}\n" unperturbed_dataset_coeffs;
    printf !"%{Complex.Rect.vector_to_string}" cs_approx;
    [%expect {||}]

  (**
    Accepts four arguments:
    * lower - the lower bound of the dataset range
    * upper - the upper bound of the dataset range
    * std - the average amount by which the data was perturbed
    * cs' = the discrete fourier transform coefficients of the
      perturbed dataset's probability density function
    and returns approximations for the discrete fourier transform
    coefficients of the original unperturbed dataset.

    Note 1: you can approximate the probability density function of
    the original unperturbed dataset by taking the inverse discrete
    fourier transform of the returned coefficients.

    WARNING: when you take the inverse discrete fourier transform of
    the returned coefficients, the resulting function may exhibit
    "ringing" (rapid oscillations around its mean). You can mitigate
    this ringing effect by applying the smooth function provided.
  *)
  let get_unperturbed_dataset_coeffs ~lower ~upper ~std cs' =
    let num_pts = Array.length cs' in
    let phi_matrix = get_phi_matrix ~num_pts ~lower ~upper ~std in
    let phi_matrix_inv = Complex.Rect.matrix_inv phi_matrix in
    let cs_approx = Complex.Rect.vector_matrix_mult phi_matrix_inv cs' in
    cs_approx

  let%expect_test "get_unperturbed_dataset_coeffs" =
    let open Float in
    let num_pts = 50 (* was 50 *)
    and lower = -40.0 (* TODO: should be (at least) -20 to 20 was -10 to 10 *)
    and upper = 40.0
    and std = 3.0 in
    let unperturbed_dataset =
      get_sample_data ~lower ~upper ~num_pts (fun x -> pdf_normal ~mean:0.0 ~std:5.0 x)
    in
    let perturbed_dataset =
      get_sample_data ~lower ~upper ~num_pts (fun x0 ->
          (Integrate.qagi () ~f:(fun x ->
               pdf_normal ~mean:0.0 ~std:5.0 x * pdf_normal ~mean:x ~std x0 ) )
            .out )
    in
    let _unperturbed_dataset_coeffs = FFT.to_coeffs_slowly unperturbed_dataset in
    let perturbed_dataset_coeffs = FFT.to_coeffs_slowly perturbed_dataset in
    let cs_approx = get_unperturbed_dataset_coeffs ~lower ~upper ~std perturbed_dataset_coeffs in
    let approx = FFT.to_data_slowly cs_approx in
    printf !"ps: %{real_vector_to_string};" unperturbed_dataset;
    printf !"qs: %{real_vector_to_string};" (Array.map approx ~f:Complex.Rect.real);
    [%expect {||}]

  (**
    Accepts lower arguments:
    * lower - the lower bound of the dataset range
    * upper - the upper bound of the dataset range
    * cs - an approximation of the original unperturbed dataset's
      probability density function as returned by `unperturb`.
    and returns a function that approximates the original unperturbed
    dataset's probability density function.

    Note: this function oscillates around the true value and should be
    passed through the `smooth` function.
  *)
  let get_unperturbed_dataset ~lower ~upper cs =
    let n = Array.length cs in
    fun x ->
      let open Complex.Rect in
      let j = Float.(of_int n * (x - lower) / (upper - lower)) in
      let acc = ref zero in
      for k = 0 to Int.((n / 2) - 1) do
        let w =
          Complex.Polar.{ r = 1.0; theta = Float.(2.0 * pi * (j - 0.5) * of_int k / of_int n) }
          |> Complex.from_polar
        in
        let c = if Int.(k = 0) then cs.(k) else cs.(k) *. 2.0 in
        acc := !acc + (c * w /. float n)
      done;
      real !acc

  (* let%expect_test "get_unperturbed_dataset" =
     let open Float in
     let num_pts = 50
     and lower = -22.0
     and upper = 22.0
     and std = 1.0 in
     [|
       pdf_normal ~mean:0.0 ~std:2.0;
       (function
       | x when -1.0 <= x && x <= 1.0 -> 1 // 2
       | _ -> 0.0);
     |]
     |> Array.iter ~f:(fun unperturbed_pdf ->
          let perturbed_dataset =
            get_sample_data ~lower ~upper ~num_pts (fun x0 ->
              (Integrate.integration_qagi ~f:(fun x -> unperturbed_pdf x * pdf_normal ~mean:x ~std x0)).out )
          in
          let unperturbed_dataset_approx =
            get_perturbed_dataset_coeffs perturbed_dataset
            |> get_unperturbed_dataset_coeffs ~lower ~upper ~std
            |> get_unperturbed_dataset ~lower ~upper
            |> get_sample ~lower ~upper ~num_pts
          in
          let unperturbed_dataset = unperturbed_pdf |> get_sample ~lower ~upper ~num_pts in
          printf
            !"%{real_matrix_to_string}\n%{real_matrix_to_string}"
            (Array.slice unperturbed_dataset 20 30)
            (Array.slice unperturbed_dataset_approx 20 30) );
     [%expect
       {|
       [
         [-4.0408, 0.0259],
         [-3.1429, 0.0580],
         [-2.2449, 0.1062],
         [-1.3469, 0.1590],
         [-0.4490, 0.1945],
         [0.4490, 0.1945],
         [1.3469, 0.1590],
         [2.2449, 0.1062],
         [3.1429, 0.0580],
         [4.0408, 0.0259]
       ]

       [
         [-4.0408, 0.0234],
         [-3.1429, 0.0547],
         [-2.2449, 0.1034],
         [-1.3469, 0.1579],
         [-0.4490, 0.1953],
         [0.4490, 0.1953],
         [1.3469, 0.1579],
         [2.2449, 0.1034],
         [3.1429, 0.0547],
         [4.0408, 0.0234]
       ]
       [
         [-4.0408, 0.0000],
         [-3.1429, 0.0000],
         [-2.2449, 0.0000],
         [-1.3469, 0.0000],
         [-0.4490, 0.5000],
         [0.4490, 0.5000],
         [1.3469, 0.0000],
         [2.2449, 0.0000],
         [3.1429, 0.0000],
         [4.0408, 0.0000]
       ]

       [
         [-4.0408, -0.0068],
         [-3.1429, 0.0105],
         [-2.2449, -0.0188],
         [-1.3469, 0.0531],
         [-0.4490, 0.5049],
         [0.4490, 0.5049],
         [1.3469, 0.0531],
         [2.2449, -0.0188],
         [3.1429, 0.0105],
         [4.0408, -0.0068]
       ] |}] *)

  (**
    Accepts a set of points, xs, and smooths those points by taking the
    mean point between pairs of adjacent points.
  *)
  let smooth ~range xs =
    let n = Array.length xs in
    let result = Queue.create ~capacity:(n / range) () in
    for i = 0 to (n / range) - 1 do
      let xsum = ref 0.0 in
      let ysum = ref 0.0 in
      for j = 0 to range - 1 do
        xsum := !xsum +. xs.((i * range) + j).(0);
        ysum := !ysum +. xs.((i * range) + j).(1)
      done;
      Queue.enqueue result [| !xsum /. float range; !ysum /. float range |]
    done;
    Queue.to_array result

  (* let%expect_testx "perturb_test" =
     let lower = -25.0
     and upper = 25.0
     and std = 2.0
     and n = 5_000 in
     let num_pts = Float.to_int (upper -. lower) in
     (* to control oscilations it appears best to set one point per unit of width. Accuracy appears to increase with increasing margins on both sides. *)
     (* let unperturbed_dataset = Array.init 1_000 ~f:(fun _ -> rand_normal ()) in *)
     let unperturbed_dataset =
       Array.init n ~f:(function
         | i when i < n / 2 -> Random.float_range (-10.0) (-5.0)
         | _ -> Random.float_range 5.0 10.0
         )
     in
     let unperturbed_dataset_pdf =
       Float.(
         function
         | x when -10.0 <= x && x <= -5.0 -> 1 // 10
         | x when 5.0 <= x && x <= 10.0 -> 1 // 10
         | _ -> 0.0
       )
     in
     let perturbed_dataset = Array.map unperturbed_dataset ~f:(fun x -> x +. (std *. rand_normal ())) in
     (* this is the "normal reference rule-of-thumb" bandwidth *)
     (* let bandwidth = 1.06 *. std *. expt (float num_pts) (-0.2) in *)
     let bandwidth = 0.1 in
     let perturbed_dataset_epdf_sample =
       (* get_epdf_sample ~bandwidth ~lower ~upper ~num_pts perturbed_dataset *)
       get_sample ~lower ~upper ~num_pts (fun x0 ->
           (Integrate.integration_qagi ~f:(fun x -> unperturbed_dataset_pdf x *. pdf_normal ~mean:x ~std x0)
           )
             .out
       )
     in
     let perturbed_dataset_coeffs =
       perturbed_dataset_epdf_sample |> Array.map ~f:(fun x -> x.(1)) |> get_perturbed_dataset_coeffs
     in
     let unperturbed_dataset_ceoffs =
       get_unperturbed_dataset_coeffs ~lower ~upper ~std perturbed_dataset_coeffs
     in
     let unperturbed_dataset_approx =
       get_unperturbed_dataset ~lower ~upper unperturbed_dataset_ceoffs
       |> get_sample ~lower ~upper ~num_pts
       |> smooth ~range:1
     in
     get_sample ~lower ~upper ~num_pts unperturbed_dataset_pdf |> printf !"xs: %{real_matrix_to_string}$\n";
     printf !"ys: %{real_matrix_to_string}$\n" perturbed_dataset_epdf_sample;
     printf !"zs: %{real_matrix_to_string}$\n" unperturbed_dataset_approx;
     ();
     [%expect {||}] *)
end

(**
  This module defines functions that can be used to group elements
  together.

  Given a set of elements E the transitive closure is a set of subsets
  called groups G that satisfy the following property:

  for every pair of elements e0 and en, if there exists a sequence
  of elements e0, e1, e2, ..., en such that every element ei in
  this sequence shares one or more key values with the following
  element, then e0 and en are in the same group.

  As a corrolary of this definition, if e0 is in a group g, g also
  contains every element that shares a key with e0.

  Why would we use this? Imagine that you have a set of people and
  every person has a list of addresses and we want to cluster together
  people who lived at the same address. We can use this module to
  group them together.
*)
module Transitive_closure = struct
  (** represents the type of elements that will be grouped together *)
  module type Element = sig
    type t

    (**
      Represents the keys that elements have. These will be used to
      determine if two elements "overlap" and should be grouped together.
    *)

    module K : sig
      type t [@@deriving compare, equal, hash, sexp]
    end

    module Set : Set.S with type Elt.t = K.t

    (** Accepts an elements and returns the keys that it is associated with. *)
    val get_keys : t -> Set.t
  end

  module Make (E : Element) = struct
    module KTable = Hashtbl.Make (E.K)

    (** Accepts a list of elements and returns their transitive closure. *)
    type t = E.t

    let get xs =
      let get_next_group_id =
        let next_group_id = ref 0 in
        fun () ->
          let x = !next_group_id in
          incr next_group_id;
          x
      in
      let key_group_tbl : int KTable.t = KTable.create () in
      let group_keys_tbl : E.Set.t Int.Table.t = Int.Table.create () in
      let group_records_tbl : E.t list Int.Table.t = Int.Table.create () in

      let make_empty_group () =
        let group_id = get_next_group_id () in
        Hashtbl.add_exn group_keys_tbl ~key:group_id ~data:E.Set.empty;
        Hashtbl.add_exn group_records_tbl ~key:group_id ~data:[];
        group_id
      in
      let add_record (x : t) keys group_id =
        Set.iter keys ~f:(fun key -> Hashtbl.set key_group_tbl ~key ~data:group_id);
        Hashtbl.update group_keys_tbl group_id ~f:(function
          | None -> keys
          | Some existing -> Set.union existing keys );
        Hashtbl.add_multi group_records_tbl ~key:group_id ~data:x
      in
      let combine_groups group_id ~into_group_id =
        let records = Hashtbl.find_and_remove group_records_tbl group_id |> Option.value_exn in
        let keys = Hashtbl.find_and_remove group_keys_tbl group_id |> Option.value_exn in
        Set.iter keys ~f:(fun key ->
            Hashtbl.update key_group_tbl key ~f:(function
              | None -> failwiths ~here:[%here] "Error: an internal error occured." () [%sexp_of: unit]
              | Some _ -> into_group_id ) );
        Hashtbl.update group_keys_tbl into_group_id ~f:(function
          | None -> failwiths ~here:[%here] "Error: an internal error occured." () [%sexp_of: unit]
          | Some existing -> Set.union existing keys );
        Hashtbl.update group_records_tbl into_group_id ~f:(function
          | None -> failwiths ~here:[%here] "Error: an internal error occured." () [%sexp_of: unit]
          | Some ll -> List.concat_no_order [ ll; records ] )
      in
      List.iter xs ~f:(fun (x : E.t) ->
          let keys = E.get_keys x in
          let group_ids =
            Set.fold keys ~init:Int.Set.empty ~f:(fun group_ids k ->
                Hashtbl.find_and_call ~if_found:(Set.add group_ids)
                  ~if_not_found:(fun (_ : KTable.key) -> group_ids)
                  key_group_tbl k )
            |> Set.to_sequence
          in
          let into_group_id = Sequence.hd group_ids |> Option.value_or_thunk ~default:make_empty_group in
          add_record x keys into_group_id;
          Sequence.drop group_ids 1 |> Sequence.iter ~f:(combine_groups ~into_group_id) );

      group_records_tbl
  end

  module Example1 = Make (struct
    type t = String.Set.t * int

    module K = String
    module Set = String.Set

    let get_keys = fst
  end)

  let%expect_test "Transitive Closure 1" =
    [ String.Set.of_list [ "A" ], 1; String.Set.of_list [ "B" ], 2 ]
    |> Example1.get
    |> Hashtbl.data
    |> printf !"%{sexp: ((String.Set.t * int) list) list}\n";
    [ String.Set.of_list [ "A" ], 1; String.Set.of_list [ "B" ], 2; String.Set.of_list [ "A"; "B" ], 3 ]
    |> Example1.get
    |> Hashtbl.data
    |> printf !"%{sexp: ((String.Set.t * int) list) list}\n";
    [
      String.Set.of_list [ "A" ], 1;
      String.Set.of_list [ "B" ], 2;
      String.Set.of_list [ "A"; "B" ], 3;
      String.Set.of_list [ "C" ], 4;
    ]
    |> Example1.get
    |> Hashtbl.data
    |> printf !"%{sexp: ((String.Set.t * int) list) list}\n";
    [%expect
      {|
      ((((B) 2)) (((A) 1)))
      ((((B) 2) ((A) 1) ((A B) 3)))
      ((((C) 4)) (((B) 2) ((A) 1) ((A B) 3))) |}]
end

module Route = struct
  module Point = struct
    type 'a t = {
      label: 'a;
      lat: float;
      lon: float;
    }
    [@@deriving equal, sexp]

    let dist p q =
      let open Float in
      sqrt (square (p.lat - q.lat) + square (p.lon - q.lon))
  end

  (**
    Accepts two arguments: [start], a point; and [points], a list of
    points; and returns the shortest path that starts with [start]
    and passes through all of the points in [points].

    WARNING: this function uses exhaustive depth-first search
    and does not scale past a few points. Note however that this
    function will return the absolute shortest path if it exists.

    Example:

    {[
    let len, path =
      find_path
        ~label_equal:String.equal
        Point.{ label = "3804 Delverne Rd, Baltimore, MD 21218"; lat = 39.33428; lon = -76.59854 }
        Point.
          [
            { label = "1517 Tunlaw Rd, Baltimore, MD 21218"; lat = 39.33511; lon = -76.59328 };
            { label = "3809 Monterey Rd, Baltimore, MD 21218"; lat = 39.33438; lon = -76.59716 };
            { label = "1527 Tunlaw Rd, Baltimore, MD 21218"; lat = 39.33512; lon = -76.59287 };
            { label = "3814 The Alameda, Baltimore, MD 21218"; lat = 39.33499; lon = -76.59688 };
            { label = "1621 Ralworth Rd, Baltimore, MD 21218"; lat = 39.3392; lon = -76.59058 };
            { label = "1533 Medford Rd, Baltimore, MD 21218"; lat = 39.33582; lon = -76.59241 };
            { label = "3912 The Alameda, Baltimore, MD 21218"; lat = 39.33665; lon = -76.59888}
          ]
    in ...
    ]}
  *)
  let find_path ~label_equal start points =
    let rec aux curr_len last_point curr_path =
      List.fold points ~init:None ~f:(fun acc point ->
          if List.mem curr_path point ~equal:(Point.equal label_equal)
          then acc
          else (
            let ((new_len, _) as new_res) =
              aux Float.(curr_len + Point.dist last_point point) point (point :: curr_path)
            in
            match acc with
            | None -> Some new_res
            | Some (other_len, _) when Float.(new_len < other_len) -> Some new_res
            | Some other -> Some other ) )
      |> function
      | None -> curr_len, curr_path
      | Some res -> res
    in
    let len, path = aux 0.0 start (List.singleton start) in
    len, List.rev path
end

(**
  This module defines functions for working with orthogonal functions. In
  particular, it defines functions that can be used to project a function
  onto an orthogonal bases such as Legendre, Hermite, Laguerre, and Chebyshev
  polynomials.
*)
module Orthogonal_functions = struct
  module Range = struct
    type t = {
      lower: float;
      upper: float;
      singularities: float array;
    }
  end

  let inner_product Range.{ lower; upper; singularities } f g =
    let res = Integrate.qagp () ~lower ~upper ~singularities ~f:(fun x -> f x *. g x) in
    res.out

  let length range f = sqrt @@ inner_product range f f

  let projection range bases f = Array.map bases ~f:(fun u -> inner_product range f u /. length range u)

  module Fourier_series = struct
    (**
      Accepts two arguments: [range] and [n]; and returns the basis functions
      for computing a fourier series approximation over the given range using
      [n+1] terms.
    *)
    let bases range n =
      let open Float in
      let range_width = Range.(range.upper - range.lower) in
      let range_middle = Range.(range.lower + (range_width / 2.0)) in
      Sequence.append
        (Sequence.singleton (Fn.const 1.0))
        (Sequence.concat
           (Sequence.init n ~f:(fun i ->
                let m = float i + 1.0 in
                Sequence.append
                  (Sequence.singleton (fun x -> cos (2.0 * pi * (x - range_middle) * m / range_width)))
                  (Sequence.singleton (fun x -> sin (2.0 * pi * (x - range_middle) * m / range_width))) )
           ) )
      |> Sequence.to_array
  end
end
