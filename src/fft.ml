open Core

open Stats

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
