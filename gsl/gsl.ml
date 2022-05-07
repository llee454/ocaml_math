external ocaml_log1p : float -> float = "ocaml_log1p"

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
end

module Integrate = struct
  type t = {
    out: float;
    err: float;
    neval: int64;
  }
  external ocaml_integrate : f:(float -> float) -> lower:float -> upper:float -> t = "ocaml_integrate"

  let f = ocaml_integrate
end

module Nonlinear_fit = struct
  (*
    represents the parameter values used by a function to process an
    argument value
  *)
  type t = {
    ks: float array; (* parameter values *)
    x:  float;       (* argument value *)
  }

  (*
    Accepts a function, f, that has parameters ks and accepts a float and returns a float; and a set of initial parameter values, ks_init; and a set of data values, xs; and returns the set of parameter values that minimize the error between f_ks (x) 
  *)
  external f : f:(t -> float) -> ks_init:(float array) -> xs:(float array) -> ys:(float array) -> float array = "ocaml_gsl_fit_nlinear"
end
