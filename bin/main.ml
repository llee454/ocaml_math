open! Core
open! Lwt.Syntax
open! Lwt.Infix
open! Asemio_stats

let () =
  Lwt_main.run
  @@
  (* I. Linear regresion *)
  let Linear_fit.{ c0; c1 } = Linear_fit.f [| 0.0; 1.0; 2.0 |] [| 5.0; 10.0; 20.0 |] in
  let* () = Lwt_io.printlf "linear regression: %f + %f x" c0 c1 in
  (* II. Numerical Integration *)
  let Integrate.{ out; err; _ } = Integrate.f ~f:(fun x -> Float.log1p x) ~lower:0.1 ~upper:0.5 in
  let* () = Lwt_io.printlf "integration: result %f err %f" out err in
  (* III. Parameter estimation using simulated annealing *)
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
  let* () = Lwt_io.printlf "simulated annealing result: %f" result in
  Lwt.return_unit
