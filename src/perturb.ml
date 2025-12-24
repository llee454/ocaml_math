(**
  WARNING: THIS MODULE IS UNSTABLE AND IS NOT FINISHED DO NOT USE!!

  This module defines functions that can be used to mask datasets by
  adding gaussian noise and to analyze these datasets by removing
  gaussian noise.
*)
open Core

open Basic
open Linalg
open Stats

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
let get_perturbed_dataset_coeffs = Fft.to_coeffs_slowly

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
  let unperturbed_dataset_coeffs = Fft.to_coeffs_slowly unperturbed_dataset in
  let perturbed_dataset_coeffs = Fft.to_coeffs_slowly perturbed_dataset in
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
  let _unperturbed_dataset_coeffs = Fft.to_coeffs_slowly unperturbed_dataset in
  let perturbed_dataset_coeffs = Fft.to_coeffs_slowly perturbed_dataset in
  let cs_approx = get_unperturbed_dataset_coeffs ~lower ~upper ~std perturbed_dataset_coeffs in
  let approx = Fft.to_data_slowly cs_approx in
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
