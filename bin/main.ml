open! Core
open! Asemio_stats

(* let () =
   Eio_main.run @@ fun env ->
   let stdout = Eio.Stdenv.stdout env in
   (* I. Linear regresion *)
   Eio.Switch.run @@ fun sw ->
   let pool = Eio.Executor_pool.create ~sw (Eio.Stdenv.domain_mgr env) ~domain_count:4 in
   let () =
     Eio.Fiber.fork ~sw @@ fun () ->
     Eio.Executor_pool.submit_exn ~weight:0.1 pool @@ fun () ->
     let Linear_fit.{ c0; c1 } = Linear_fit.f [| 0.0; 1.0; 2.0 |] [| 5.0; 10.0; 20.0 |] in
     Eio.Flow.copy_string (sprintf "linear regression: %f + %f x\n" c0 c1) stdout
   in
   (* II. Numerical Integration *)
   let () =
     Eio.Fiber.fork ~sw @@ fun () ->
     Eio.Executor_pool.submit_exn ~weight:0.1 pool @@ fun () ->
     let Integrate.{ out; err; _ } = Integrate.f ~f:(fun x -> Float.log1p x) ~lower:0.1 ~upper:0.5 in
     Eio.Flow.copy_string (sprintf "integration: result %f err %f\n" out err) stdout
   in
   (* Note: Random.self_init is not thread safe. You must call this function before you make any parallel calls to Random.float *)
   Random.self_init ();
   (* III. Parameter estimation using simulated annealing *)
   let () =
     Eio.Fiber.fork ~sw @@ fun () ->
     Eio.Executor_pool.submit_exn ~weight:1.0 pool @@ fun () ->
     let result =
       let module SA = Simulated_annealing (struct
         type t = float

         let copy x = x

         let energy x = (x +. 3.0) *. (x +. 7.0)

         let step x dist = x +. Random.float (2.0 *. dist) -. dist

         let dist x y = Float.abs (x -. y)

         let print = None
       end) in
       SA.(f ~num_iters:1_000 ~step_size:1.0 (create_state (-3.0)))
     in
     Eio.Flow.copy_string (sprintf "simulated annealing example 1 result: %f\n" result) stdout
   in
   let () =
     (* IV. Parameter estimation using simulated annealing *)
     Eio.Fiber.fork ~sw @@ fun () ->
     Eio.Executor_pool.submit_exn ~weight:1.0 pool @@ fun () ->
     let result =
       let module SA = Simulated_annealing (struct
         type t = float

         let copy x = x

         let energy x = (x +. 7.0) *. (x +. 7.0)

         let step x dist = x +. Random.float (2.0 *. dist) -. dist

         let dist x y = Float.abs (x -. y)

         let print = None
       end) in
       SA.(f ~num_iters:1_000 ~step_size:1.0 (create_state 5.0))
     in
     Eio.Flow.copy_string (sprintf "simulated annealing example 2 result: %f\n" result) stdout
   in
   () *)

let get_cont_fourier_k ~f eps =
  Complex.Integrate.qagi ~f:(fun x ->
      Complex.from_polar @@ Complex.Polar.{ r = f x; theta = Float.(-2.0 * pi * eps * x) } )

let test_fn x = Float.(exp (-square x) * sin (2.0 * pi * x / 8.0))

let perturbed_test_fn ~std x =
  (Integrate.h ~f:(fun x0 -> Float.(test_fn x0 * pdf_normal ~mean:x ~std x0))).out

let get_test_fn_k eps = get_cont_fourier_k ~f:test_fn eps

let get_perturbed_test_fn_k ~std eps = get_cont_fourier_k eps ~f:(perturbed_test_fn ~std)

let get_k_corr ~std eps = Float.(exp (2.0 * square (pi * eps * std)))

let get_est_test_fn_k ~std eps = Complex.Rect.(get_perturbed_test_fn_k ~std eps *. get_k_corr ~std eps)

let test_cont_k_corr stdout =
  let eps = 0.5
  and std = 0.3 in
  let test_fn_k = get_test_fn_k eps
  and perturbed_test_fn_k = get_perturbed_test_fn_k ~std eps
  and est_test_fn_k = get_est_test_fn_k ~std eps in
  Eio.Flow.copy_string
    (sprintf
       !"test fn k: %{Complex.Rect.to_string}\n\
         est test fn k: %{Complex.Rect.to_string}\n\
         perturbed test fn k: %{Complex.Rect.to_string}\n"
       test_fn_k est_test_fn_k perturbed_test_fn_k )
    stdout

(* next question: does the discrete fourier transform approximate the continuous forier transform? *)

let get_delta ~lower ~upper n =
  if n <= 1
  then
    failwiths ~here:[%here] "Error: get_delta failed. Invalid number of data points requested." n
      [%sexp_of: int];
  Float.((upper - lower) / (float n - 1.0))

let get_x ~lower ~delta index = Float.((float index * delta) + lower)

let get_test_fn_xs ~lower ~delta n = Array.init n ~f:(fun index -> test_fn @@ get_x ~lower ~delta index)

let test stdout =
  let lower = -4.0
  and upper = 4.0
  and n = 10 in
  let delta = get_delta ~lower ~upper n in
  let xs = get_test_fn_xs ~lower ~delta n in
  let dks = FFT.to_coeffs_slowly xs in
  Eio.Flow.copy_string
    (sprintf
       !"delta: %0.04f\n\
         xs: %{real_vector_to_string}\n\
         dk: %{Complex.Rect.to_string}\n\
         ck: %{Complex.Rect.to_string}\n"
       delta xs dks.(1)
       (get_test_fn_k (1 // 8)) )
    stdout

let () =
  Eio_main.run @@ fun env ->
  let stdout = Eio.Stdenv.stdout env in
  (* I. Linear regresion *)
  Eio.Switch.run @@ fun sw ->
  let pool = Eio.Executor_pool.create ~sw (Eio.Stdenv.domain_mgr env) ~domain_count:4 in
  let () =
    Eio.Fiber.fork ~sw @@ fun () ->
    Eio.Executor_pool.submit_exn ~weight:0.1 pool @@ fun () -> test stdout
  in
  ()
