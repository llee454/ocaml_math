open Core

type t = {
  out: float;
  err: float;
  neval: int;
}
[@@deriving sexp]

module QNG_params = struct
  type t = {
    epsabs: float;
    epsrel: float;
  }
  [@@deriving sexp]
end

module QAG_params = struct
  type t = {
    epsabs: float;
    epsrel: float;
    limit: int;
  }
  [@@deriving sexp]
end

external integration_qng : params:QNG_params.t -> f:(float -> float) -> lower:float -> upper:float -> t
  = "ocaml_integration_qng"

let qng ?(params : QNG_params.t = QNG_params.{ epsabs = 0.0; epsrel = 1e-2 }) ~f ~lower ~upper () =
  integration_qng ~params ~f ~lower ~upper

let%expect_test "Integrate.qng" =
  let { out; _ } =
    qng ~lower:(-1.0) ~upper:1.0 ~f:Float.((fun x -> exp ~-(square x /. 2.0) /. (2.0 *. sqrt_pi))) ()
  in
  printf "%.4f" out;
  [%expect {|0.4827|}]

let%expect_test "Integrate.qng" =
  qng () ~lower:0.0 ~upper:100.0
    ~f:(fun x ->
        let open Float in
        (2.0*(sqrt x)*(exp (-x/2.0)))/(2.0*(sqrt(2.0*pi)))
      )
  |> printf !"%{sexp: t}";
  [%expect {| ((out 1.0000046750749449) (err 0.0019604246975912119) (neval 87)) |}]

external integration_qag : params:QAG_params.t -> f:(float -> float) -> lower:float -> upper:float -> t
  = "ocaml_integration_qag"

let qag ?(params : QAG_params.t = QAG_params.{ epsabs = 0.0; epsrel = 1e-9; limit = 10_000 }) ~f ~lower
  ~upper () =
  integration_qag ~params ~f ~lower ~upper

let%expect_test "Integrate.gag" =
  let { out; _ } =
    qag ~lower:(-1.0) ~upper:1.0 ~f:Float.((fun x -> exp ~-(square x /. 2.0) /. (2.0 *. sqrt_pi))) ()
  in
  printf "%.4f" out;
  [%expect {|0.4827|}]

let%expect_test "Integrate.qag" =
  qag () ~lower:0.0 ~upper:100.0
    ~f:(fun x ->
        let open Float in
        (2.0*(sqrt x)*(exp (-x/2.0)))/(2.0*(sqrt(2.0*pi)))
      )
  |> printf !"%{sexp: t}";
  [%expect {| ((out 1.0000000000046587) (err 9.6387226193344787E-10) (neval 0)) |}]

external integration_qagi : params:QAG_params.t -> f:(float -> float) -> t = "ocaml_integration_qagi"

let qagi ?(params : QAG_params.t = QAG_params.{ epsabs = 0.0; epsrel = 1e-9; limit = 10_000 }) ~f () = integration_qagi ~params ~f

let%expect_test "Integrate.qagi" =
  let { out; _ } = qagi () ~f:Float.((fun x -> 2.0 ** ~-(square x))) in
  printf "%.8f" out;
  [%expect {| 2.12893404 |}]

external integration_qagp :
  params:QAG_params.t ->
  f:(float -> float) ->
  lower:float ->
  upper:float ->
  singularities:float array ->
  t = "ocaml_integration_qagp"

let qagp ?(params : QAG_params.t = QAG_params.{ epsabs = 0.0; epsrel = 1e-9; limit = 10_000 }) ~f ~lower
  ~upper ~singularities () =
  integration_qagp ~params ~f ~lower ~upper ~singularities

let%expect_test "Integrate.qagp" =
  qag () ~lower:0.0 ~upper:100.0
    ~f:(fun x ->
        let open Float in
        (2.0*(sqrt x)*(exp (-x/2.0)))/(2.0*(sqrt(2.0*pi)))
      )
  |> printf !"%{sexp: t}";
  [%expect {| ((out 1.0000000000046587) (err 9.6387226193344787E-10) (neval 0)) |}]
