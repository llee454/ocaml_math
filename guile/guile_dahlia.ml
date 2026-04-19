open! Core
open! Ocaml_math

type c_real_fn

external call_scm_real_fn : c_real_fn -> float -> float = "call_scm_real_fn"

let int_test (fn : c_real_fn) =
  (Integrate.qag
    ~params:Integrate.QAG_params.{
      epsabs = 1e-5;
      epsrel = 1e-5;
      limit  = 1_000
    }
    ~f:(call_scm_real_fn fn)
    ~lower:0.0
    ~upper:1.0
    ()).out

let _ =
  Callback.register "int_test" int_test
