open! Core
open! Ocaml_math

type scm_real_fn

external of_scm_fn : scm_real_fn -> float -> float = "apply_scm_real_fn"

let integrate_qng params (fn : scm_real_fn) lower upper =
  Integrate.qng
    ~params
    ~f:(of_scm_fn fn)
    ~lower
    ~upper

let integrate_qag params (fn : scm_real_fn) lower upper =
  Integrate.qag
    ~params
    ~f:(of_scm_fn fn)
    ~lower
    ~upper

let _ =
  Callback.register "integrate_qng" integrate_qng;
  Callback.register "integrate_qag" integrate_qag
