open Core

let sum ~f = Array.fold ~init:0.0 ~f:(fun sum x -> sum +. (f x))

external pow_int : float -> int -> float = "ocaml_gsl_pow_int"

external mean : float array -> float = "ocaml_mean"

external cdf_gaussian_p : x:float -> std:float -> float = "ocaml_gsl_cdf_gaussian_P"

external cdf_chi_squared_p : x:float -> ndf:float -> float = "ocaml_gsl_cdf_chisq_P"

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

module Stats_tests = struct

  exception N_too_small

  (*
    Computes the Jarque-Bera statistic for the given dataset.

    Note: for n >= 2,000 the distribution of the JB statistic
    approximates a X^2 (Chi-squared) distribution with 2 degrees of
    freedom.

    Warning: this function will throw the N_too_small exception if the
    dataset is smaller than 2,000.

    References:
    1. "The Shapiro-Wilk And Related Tests for Normality",
       [https://math.mit.edu/~rmd/465/shapiro.pdf]
  *)
  let jarque_bera ~xs =
    let n =
      let n = Array.length xs in
      if n < 2000 then raise N_too_small;
      float n
    in
    let u = mean xs in
    (* compute the biased sample deviation *)
    let s = sqrt ((sum ~f:(fun x -> Float.square (x -. u)) xs) /. n) in
    let skew = (sum ~f:(fun x -> (pow_int ((x -. u) /. s) 3)) xs) /. n in
    let kurt = ((sum ~f:(fun x -> (pow_int ((x -. u) /. s) 4)) xs) /. n) -. 3.0 in
    n /. 6.0 *. (Float.square (skew) +. Float.square (kurt) /. 4.0)
end
