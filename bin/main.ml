open! Core
open! Lwt.Syntax
open! Lwt.Infix
open! Asemio_stats

let main =
  Lwt_main.run
  @@
  (* let Linear_fit.{ c0; c1 } = Linear_fit.f [| 0.0; 1.0; 2.0 |] [| 5.0; 10.0; 20.0 |] in
  let* () = Lwt_io.printlf "linear regression: %f + %f x" c0 c1 in
  let Integrate.{ out; err; _ } = Integrate.f ~f:(fun x -> Float.log1p x) ~lower:0.1 ~upper:0.5 in
  let* () = Lwt_io.printlf "integration: result %f err %f" out err in *)
  let result =
    let module SA = Simulated_annealing (struct
      type t = float

      let copy x =
        (* sprintf "[copy] x = %0.4f" x |> print_endline; *)
        x

      let energy x =
        (* sprintf "[energy] x = %0.4f" x |> print_endline; *)
        (x +. 7.0) *. (x +. 7.0)

      let step x dist =
        (* sprintf "[step] x = %0.4f dist = %0.4f" x dist |> print_endline; *)
        x +. Random.float (2.0 *. dist) -. dist

      let dist x y =
        (* sprintf "[dist] x = %0.4f y = %0.4f" x y |> print_endline; *)
        Float.abs (x -. y)

      let print = None (* Some (fun x -> print_endline @@ sprintf "state: %0.4f" x) *)
    end) in
    SA.(f ~num_iters:50 ~step_size:1.0 (create_state 5.0))
  in
  let* () = Lwt_io.printlf "simulated annealing result: %f" result in
  Lwt.return_unit
