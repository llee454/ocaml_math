open Core  

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
