open Core

open Basic
open Linalg

module Polar = struct
  type t = {
    r: float;
    theta: float;
  }
  [@@deriving fields, sexp]
end

module Rect = struct
  type t = {
    real: float;
    imag: float;
  }
  [@@deriving fields, sexp]

  let to_string { real; imag } = sprintf "%0.4f + %0.4fi" real imag

  let zero = { real = 0.0; imag = 0.0 }

  let i = { real = 0.0; imag = 1.0 }

  external ( + ) : t -> t -> t = "ocaml_complex_add"

  let%expect_test "Complex.Rect.add" =
    { real = 2.0; imag = -3.0 } + { real = 1.0; imag = 2.0 } |> printf !"%{sexp: t}";
    [%expect {| ((real 3) (imag -1)) |}]

  external ( - ) : t -> t -> t = "ocaml_complex_sub"

  let%expect_test "Complex.Rect.sub" =
    { real = 2.0; imag = -3.0 } - { real = 1.0; imag = 2.0 } |> printf !"%{sexp: t}";
    [%expect {| ((real 1) (imag -5)) |}]

  external ( * ) : t -> t -> t = "ocaml_complex_mul"

  let%expect_test "Complex.Rect.mul" =
    { real = 2.0; imag = -3.0 } * { real = 1.0; imag = 2.0 } |> printf !"%{sexp: t}";
    [%expect {| ((real 8) (imag 1)) |}]

  let ( +. ) x k = { real = k +. x.real; imag = x.imag }

  let ( -. ) x k = { real = x.real -. k; imag = x.imag }

  let ( *. ) x k = { real = k *. x.real; imag = k *. x.imag }

  external ( / ) : t -> t -> t = "ocaml_complex_div"

  let%expect_test "Complex.Rect.div" =
    { real = 2.0; imag = -3.0 } / { real = 1.0; imag = 2.0 } |> printf !"%{sexp: t}";
    [%expect {| ((real -0.8) (imag -1.4)) |}]

  let ( /. ) x k = { real = x.real /. k; imag = x.imag /. k }

  let mag { real; imag } = Float.(sqrt (square real + square imag))

  let matrix_to_string = matrix_to_string (fun z -> sprintf "(%0.4f, %0.4f)" z.real z.imag)

  let vector_to_string = vector_to_string (fun z -> sprintf "(%0.4f, %0.4f)" z.real z.imag)

  external matrix_inv : t array array -> t array array = "ocaml_gsl_matrix_complex_inv"

  let%expect_test "Complex.Rect.matrix_inv" =
    [|
      [| { real = 1.0; imag = 2.0 }; { real = 3.0; imag = 4.0 } |];
      [| { real = 5.0; imag = 6.0 }; { real = 7.0; imag = 8.0 } |];
    |]
    |> matrix_inv
    |> printf !"%{sexp: t array array}";
    [%expect
      {|
      ((((real -0.49999999999999989) (imag 0.43749999999999994))
        ((real 0.24999999999999994) (imag -0.18750000000000003)))
       (((real 0.37500000000000006) (imag -0.31250000000000006))
        ((real -0.12500000000000003) (imag 0.062500000000000028))))
      |}]

  external matrix_det : t array array -> t = "ocaml_gsl_matrix_complex_det"

  let%expect_test "Complex.Rect.matrix_det" =
    [|
      [| { real = 1.0; imag = 1.0 }; { real = 2.0; imag = 2.0 } |];
      [| { real = 3.0; imag = 3.0 }; { real = 4.0; imag = 4.0 } |];
    |]
    |> matrix_det
    |> printf !"%{to_string}";
    [%expect {| 0.0000 + -4.0000i |}]

  let vector_inner_product = Array.fold2_exn ~init:zero ~f:(fun acc x y -> acc + (x * y))

  let vector_matrix_mult (m : t array array) (x : t array) =
    Array.map m ~f:(fun row -> vector_inner_product row x)

  let matrix_mult m0 m1 =
    let n = Array.length m1
    and nrows = Array.length m0 in
    if n = 0 || nrows = 0
    then
      failwiths ~here:[%here]
        "Error: an error occurred while trying to multiply two complex matrices. One or both of these \
         matrices were empty."
        () [%sexp_of: unit];
    if n <> Array.length m0.(0)
    then
      failwiths ~here:[%here]
        "Error: an error occured while trying to multiply two complex matrices. The matrices are not \
         compatible."
        () [%sexp_of: unit];
    let ncols = Array.length m1.(0) in
    let result = Array.make_matrix ~dimx:nrows ~dimy:ncols zero in
    for i = 0 to Int.(nrows - 1) do
      for j = 0 to Int.(ncols - 1) do
        let acc = ref zero in
        for k = 0 to Int.(n - 1) do
          acc := !acc + (m0.(i).(k) * m1.(k).(j))
        done;
        result.(i).(j) <- !acc
      done
    done;
    result

  let%expect_test "matrix_mult" =
    let m1 =
      [|
        [| { real = 1.0; imag = 0.0 }; { real = 2.0; imag = 0.0 } |];
        [| { real = 3.0; imag = 0.0 }; { real = 4.0; imag = 0.0 } |];
      |]
    in
    let m2 =
      [|
        [| { real = 1.0; imag = 0.0 }; { real = 2.0; imag = 0.0 } |];
        [| { real = 3.0; imag = 0.0 }; { real = 4.0; imag = 0.0 } |];
      |]
    in
    matrix_mult m1 m2 |> printf !"%{sexp: t array array}";
    [%expect
      {|
      ((((real 7) (imag 0)) ((real 10) (imag 0)))
       (((real 15) (imag 0)) ((real 22) (imag 0)))) |}]

  let frobenius_norm m = Float.sqrt (sumi m ~f:(fun _i -> sumi ~f:(fun _j z -> Float.square (mag z))))

  let%expect_test "frobenius_norm" =
    frobenius_norm
      [|
        [| { real = 1.0; imag = 0.0 }; { real = -2.0; imag = 0.0 }; { real = 0.0; imag = 0.0 } |];
        [| { real = -1.0; imag = 0.0 }; { real = 3.0; imag = 0.0 }; { real = 2.0; imag = 0.0 } |];
        [| { real = 1.0; imag = 0.0 }; { real = -1.0; imag = 0.0 }; { real = 1.0; imag = 0.0 } |];
      |]
    |> printf "%0.4f";
    [%expect {| 4.6904 |}]
end

external from_polar : Polar.t -> Rect.t = "ocaml_from_polar"

let%expect_test "Complex.from_polar" =
  let open Float in
  Polar.[| { r = 2.0; theta = pi }; { r = 1.0; theta = 2.0 * pi } |]
  |> Array.map ~f:from_polar
  |> printf !"%{sexp: Rect.t array}";
  [%expect
    {|
    (((real -2) (imag 2.4492935982947064E-16))
     ((real 1) (imag -2.4492935982947064E-16))) |}]

module Integrate = struct
  let qag ~lower ~upper ~f =
    Rect.
      {
        real = (Integrate.qag () ~lower ~upper ~f:(fun x -> (f x).real)).out;
        imag = (Integrate.qag () ~lower ~upper ~f:(fun x -> (f x).imag)).out;
      }

  let qagi ~f =
    Rect.
      {
        real = (Integrate.qagi () ~f:(fun x -> (f x).real)).out;
        imag = (Integrate.qagi () ~f:(fun x -> (f x).imag)).out;
      }

  let qagp ~lower ~upper ~singularities ~f =
    Rect.
      {
        real = (Integrate.qagp () ~lower ~upper ~singularities ~f:(fun x -> (f x).real)).out;
        imag = (Integrate.qagp () ~lower ~upper ~singularities ~f:(fun x -> (f x).imag)).out;
      }
end
