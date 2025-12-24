open Core

external ocaml_deriv_central : f:(float -> float) -> x:float -> h:float -> float = "ocaml_deriv_central"

let f = ocaml_deriv_central

(** Returns the nth derivative of the given function at the given point using an initial step size of h. *)
let rec nth ~f:(g : float -> float) ~(x : float) ~(h : float) = function
| n when n <= 1 -> f ~f:g ~x ~h
| n -> f ~f:(fun x -> nth ~f:g ~x ~h (n - 1)) ~x ~h
