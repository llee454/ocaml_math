open Core

type vector = float array [@@deriving compare, equal, sexp]

type matrix = float array array [@@deriving compare, equal, sexp]

let vector_to_string to_string ?(indent = 0) xs =
  String.make indent ' ' ^ "[" ^ (Array.map xs ~f:to_string |> String.concat_array ~sep:", ") ^ "]\n"

let real_vector_to_string = vector_to_string (sprintf "%0.4f")

let vector_scalar_mult x = Array.map ~f:(( *. ) x)

let vector_add = Array.map2_exn ~f:( +. )

let vector_sub = Array.map2_exn ~f:( -. )

let vector_inner_product = Array.fold2_exn ~init:0.0 ~f:(fun acc x0 y0 -> acc +. (x0 *. y0))

let%expect_test "vector_inner_product_1" =
  printf "%.2f" (vector_inner_product [| 3.5; 2.0; 1.7 |] [| 4.0; 1.0; 2.8 |]);
  [%expect {|20.76|}]

let%expect_test "vector_inner_product_2" =
  printf "%.1f" (vector_inner_product [| 3.5 |] [| -4.0 |]);
  [%expect {|-14.0|}]

let vector_norm x = sqrt (vector_inner_product x x)

let%expect_test "vector_norm_1" =
  printf "%.1f" (vector_norm [| 1.0; 0.0; 0.0 |]);
  [%expect {|1.0|}]

let%expect_test "vector_norm_2" =
  printf "%.1f" (vector_norm [| -1.0 /. sqrt 2.0; 1.0 /. sqrt 2.0 |]);
  [%expect {|1.0|}]

let%expect_test "vector_norm_3" =
  printf "%.1f" (vector_norm [| -3.0 /. sqrt 2.0; 3.0 /. sqrt 2.0 |]);
  [%expect {|3.0|}]

let vector_matrix_mult (m : float array array) (x : float array) =
  let nrows = Array.length m in
  if [%equal: int] nrows 0 then [||] else Array.init nrows ~f:(fun r -> vector_inner_product m.(r) x)

let%expect_test "vector_matrix_mult" =
  let x = vector_matrix_mult [| [| 3.5; 2.0; 1.7 |]; [| 4.0; 1.0; 2.8 |] |] [| -3.4; 1.8; 5.0 |] in
  printf !"%.2f %.2f" x.(0) x.(1);
  [%expect {|0.20 2.20|}]

(**
  Accepts three arguments: a matrix, a column vector, and a result
  matrix; left multiplies m and x; and writes the result into the
  result matrix.

  Note: this function operates in constant memory and does not make
  any memory allocations.
*)
let vector_matrix_mult_cm (m : float array array) (x : float array) (res : float array) =
  let nrows = Array.length m in
  for i = 0 to nrows - 1 do
    res.(i) <- vector_inner_product m.(i) x
  done

let%expect_test "vector_matrix_mult_cm" =
  let x = Array.create_float_uninitialized ~len:2 in
  vector_matrix_mult_cm [| [| 3.5; 2.0; 1.7 |]; [| 4.0; 1.0; 2.8 |] |] [| -3.4; 1.8; 5.0 |] x;
  printf !"%.2f %.2f" x.(0) x.(1);
  [%expect {|0.20 2.20|}]

(** Returns a random vector of length [len] of dimension [dim]. *)
let create_random_vector ?(len = 1.0) dim =
  let p = Array.init dim ~f:(fun _ -> Random.float_range (-1.0) 1.0) in
  let norm = vector_norm p in
  vector_scalar_mult (len /. norm) p

let%expect_test "create_random_vector" =
  let v = create_random_vector ~len:2.0 3 in
  printf !"%{sexp: float array} %f %d" v (vector_norm v) (Array.length v);
  [%expect {| (0.65236902224161786 1.3618415123765957 1.3114123508596915) 2.000000 3 |}]

(** Returns a random vector of dimension [dim] and length less than [len]. *)
let create_random_vector_lt ?(len = 1.0) dim =
  create_random_vector ~len dim |> vector_scalar_mult (Random.float 1.0)

let%expect_test "create_random_vector_lt" =
  let v = create_random_vector_lt ~len:5.0 3 in
  printf !"%{sexp: float array} %f %d" v (vector_norm v) (Array.length v);
  [%expect {| (0.79451237416556775 1.658570374046388 1.5971533056725697) 2.435776 3 |}]

let get_mean_vector vectors =
  let (n, xs) = Array.fold vectors ~init:(0, None) ~f:(fun (n, acc_opt) vector ->
    (succ n, Some (Option.value_map acc_opt ~default:vector ~f:(vector_add vector)))
  ) in
  Option.map xs ~f:(Array.map ~f:(fun x -> x /. float n))
  

(** Accepts two points [p] and [q] and returns the distance between them. *)
let distance p q = vector_sub p q |> vector_norm 

let%expect_test "distance" =
  [
    [|0.0; 0.0; 0.0|], [|0.0; 0.0; 0.0|];
    [|1.0; 2.0; 3.0|], [|1.0; 2.0; 3.0|];
    [|1.0; 0.0; 0.0|], [|2.0; 0.0; 0.0|];
    [|1.0; 0.0|], [|0.0; 1.0|];
  ]
  |> List.map ~f:(fun (p, q) -> distance p q)
  |> printf !"%{sexp: float list}";
  [%expect {| (0 0 1 1.4142135623730951) |}]

let matrix_to_string to_string ?(indent = 0) m =
  let buffer = Buffer.create 100 in
  let tab depth s =
    String.make depth ' ' |> Buffer.add_string buffer;
    Buffer.add_string buffer s
  in
  tab indent "[\n";
  Array.map m ~f:(fun row ->
      String.make (indent + 2) ' '
      ^ "["
      ^ (Array.map row ~f:to_string |> String.concat_array ~sep:", ")
      ^ "]" )
  |> String.concat_array ~sep:",\n"
  |> Buffer.add_string buffer;
  tab indent "\n]\n";
  Buffer.contents buffer

let real_matrix_to_string = matrix_to_string (sprintf "%0.4f")

(** Accepts two matrices and adds them together. *)
let matrix_add = Array.map2_exn ~f:vector_add

let%expect_test "matrix_add" =
  let m1 = [| [| 1.0; 1.0 |]; [| 3.0; 4.0 |] |] in
  let m2 = [| [| 6.0; 7.0 |]; [| 8.0; 9.0 |] |] in
  matrix_add m1 m2 |> printf !"%{sexp: float array array}";
  [%expect {| ((7 8) (11 13)) |}]

let matrix_sub m0 m1 =
  if Array.length m0 <> Array.length m1
  then failwiths ~here:[%here] "Error: invalid dimensions." () [%sexp_of: unit];
  Array.map2_exn m0 m1 ~f:(Array.map2_exn ~f:( -. ))

let%expect_test "matrix_sub" =
  let m1 = [| [| 1.0; 1.0 |]; [| 3.0; 4.0 |] |] in
  let m2 = [| [| 6.0; 7.0 |]; [| 8.0; 9.0 |] |] in
  matrix_sub m1 m2 |> printf !"%{sexp: float array array}";
  [%expect {| ((-5 -6) (-5 -5)) |}]

let matrix_scalar_mult m x = Array.map m ~f:(Array.map ~f:(( *. ) x))

let%expect_test "matrix_scalar_mult" =
  let m1 = [| [| 1.0; 3.0 |]; [| 5.0; 7.0 |] |] in
  matrix_scalar_mult m1 3.0 |> printf !"%{sexp: float array array}";
  [%expect {| ((3 9) (15 21)) |}]

external matrix_mult : float array array -> float array array -> float array array = "ocaml_matrix_mult"

let%expect_test "matrix_mult" =
  let m1 = [| [| 1.0; 1.0; 2.0 |]; [| 3.0; 4.0; 5.0 |] |] in
  let m2 = [| [| 6.0; 7.0 |]; [| 8.0; 9.0 |]; [| 10.0; 11.0 |] |] in
  matrix_mult m1 m2 |> printf !"%{sexp: float array array}";
  [%expect {|((34 38) (100 112))|}]

let matrix_transpose (xs : float array array) : float array array =
  let nrows = Array.length xs in
  if [%equal: int] nrows 0
  then [||]
  else (
    let ncols = Array.length xs.(0) in
    Array.init ncols ~f:(fun j -> Array.init nrows ~f:(fun i -> xs.(i).(j))) )

let%expect_test "matrix_transpose" =
  let xs = [| [| 1.0; 1.0; 2.0 |]; [| 3.0; 4.0; 5.0 |] |] in
  matrix_transpose xs |> printf !"%{sexp: float array array}";
  [%expect {|((1 3) (1 4) (2 5))|}]

(** Accepts a two by two matrix and returns its determinant *)
let matrix_22_det (m : float array array) = Float.((m.(0).(0) * m.(1).(1)) - (m.(0).(1) * m.(1).(0)))

let%expect_test "matrix_22_det" =
  matrix_22_det [| [| 3.0; 7.0 |]; [| 1.0; -4.0 |] |] |> printf !"%{sexp: float}";
  [%expect {| -19 |}]

external matrix_det : float array array -> float = "ocaml_gsl_matrix_det"

let%expect_test "matrix_det" =
  matrix_det [| [| 3.0; 7.0 |]; [| 1.0; -4.0 |] |] |> printf !"%{sexp: float}";
  [%expect {| -19 |}]

let%expect_test "matrix_det" =
  matrix_det [| [| 1.0; 2.0; 3.0 |]; [| 2.0; 5.0; 6.0 |]; [| 3.0; 6.0; 9.0 |] |]
  |> printf !"%{sexp: float}";
  [%expect {| -0 |}]

let%expect_test "matrix_det" =
  matrix_det [| [| 1.0; 2.0; 3.0 |]; [| -2.0; 5.0; 6.0 |]; [| -3.0; -6.0; 9.0 |] |]
  |> printf !"%{sexp: float}";
  [%expect {| 162 |}]

external matrix_inv : float array array -> float array array = "ocaml_gsl_matrix_inv"

let%expect_test "matrix_inv" =
  let m = [| [| 6.0; 7.0 |]; [| 8.0; 9.0 |] |] in
  matrix_inv m |> printf !"%{sexp: float array array}";
  [%expect {| ((-4.5 3.5) (4 -3)) |}]

(** An optimized function to invert 2x2 matrices *)
let matrix_22_inv m =
  let open Float in
  let det = matrix_22_det m in
  let res = Array.make_matrix ~dimx:2 ~dimy:2 0.0 in
  res.(0).(0) <- m.(1).(1) / det;
  res.(0).(1) <- -m.(0).(1) / det;
  res.(1).(0) <- -m.(1).(0) / det;
  res.(1).(1) <- m.(0).(0) / det;
  res

let%expect_test "matrix_22_inv" =
  let m = [| [| 6.0; 7.0 |]; [| 8.0; 9.0 |] |] in
  matrix_22_inv m |> printf !"%{sexp: float array array}";
  [%expect {| ((-4.5 3.5) (4 -3)) |}]
