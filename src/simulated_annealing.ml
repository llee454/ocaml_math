open Core

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
