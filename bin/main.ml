open! Core
open! Asemio_stats

let () =
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
  (* WARNING: the current version of Eio intermittently throws an exception when the following calls to siman are run in parallel. *)
  let siman_mutex = Eio.Mutex.create () in
  (* III. Parameter estimation using simulated annealing *)
  let () =
    Eio.Fiber.fork ~sw @@ fun () ->
    Eio.Mutex.use_ro siman_mutex @@ fun () ->
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
    Eio.Mutex.use_ro siman_mutex @@ fun () ->
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
  ()
