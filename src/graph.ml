(**
  This module defines functions for analyzing graphs using Spectral Graph
  Theory (SGT). SGT defines algorithms for identifying disconnected subgraphs,
  clusters, and minimum cuts within graphs. It also provides methods for
  projecting graphs into R^n space.

  References:
  1. Koren, Yehuda. "On Spectral Graph Drawing." (Retreived 2025).
    https://www.mathe2.uni-bayreuth.de/axel/papers/
    koren:on_spectral_graph_drawing.pdf
*)
open Core
open Linalg

(** Accepts an association matrix and returns the corresponding degree matrix. *)
let get_degree_matrix amatrix =
  let n = Array.length amatrix in
  if [%equal: int] n 0
  then [||]
  else
    let dmatrix = Array.make_matrix ~dimx:n ~dimy:n 0.0 in
    for i = 0 to n - 1 do
      dmatrix.(i).(i) <- Array.sum (module Float) amatrix.(i) ~f:Fn.id
    done;
    dmatrix

let%expect_test "get_degree_matrix" =
  let amatrix = [|
    [| 0.0;  1.0;  0.0;  0.0;  1.0;  0.0 |];
    [| 1.0;  0.0;  1.0;  0.0;  1.0;  0.0 |];
    [| 0.0;  1.0;  0.0;  1.0;  0.0;  0.0 |];
    [| 0.0;  0.0;  1.0;  0.0;  1.0;  1.0 |];
    [| 1.0;  1.0;  0.0;  1.0;  0.0;  0.0 |];
    [| 0.0;  0.0;  0.0;  1.0;  0.0;  0.0 |]
  |] in
  get_degree_matrix amatrix
  |> printf !"%{sexp: matrix}\n";
  [%expect {|
    ((2 0 0 0 0 0) (0 3 0 0 0 0) (0 0 2 0 0 0) (0 0 0 3 0 0) (0 0 0 0 3 0)
     (0 0 0 0 0 1))
    |}]

(** Accepts an association matrix and returns the corresponding Laplacian matrix. *)
let get_laplacian_matrix amatrix =
  matrix_sub (get_degree_matrix amatrix) amatrix

let%expect_test "get_laplacian_matrix" =
  let amatrix = [|
    [| 0.0;  1.0;  0.0;  0.0;  1.0;  0.0 |];
    [| 1.0;  0.0;  1.0;  0.0;  1.0;  0.0 |];
    [| 0.0;  1.0;  0.0;  1.0;  0.0;  0.0 |];
    [| 0.0;  0.0;  1.0;  0.0;  1.0;  1.0 |];
    [| 1.0;  1.0;  0.0;  1.0;  0.0;  0.0 |];
    [| 0.0;  0.0;  0.0;  1.0;  0.0;  0.0 |]
  |] in
  get_laplacian_matrix amatrix
  |> printf !"%{sexp: matrix}\n";
  [%expect {|
    ((2 -1 0 0 -1 0) (-1 3 -1 0 -1 0) (0 -1 2 -1 0 0) (0 0 -1 3 -1 -1)
     (-1 -1 0 -1 3 0) (0 0 0 -1 0 1))
    |}]

(**
  Accepts a graph's Laplacian matrix [lmatrix] and returns the number of
  disconnected subgraphs within it.

  Note: if given, this function will count every eigenvalue whose magnitude
  is less than or equal to [threshold] as effectively 0.

  Note: this function uses the result from Graph Theory that says that the
  multiplicity of the 0 eigenvalue gives the number of disconnected subgraphs
  within a graph.
*)
let get_num_disjoint_subgraphs ?(threshold = 0.0) lmatrix =
   let open Eigen.Symmetric in
   let { values; vectors = _ } = get_eigenvectors lmatrix in
   Array.count values ~f:(fun eval -> Float.(eval <= threshold))

let%expect_test "get_num_disjoint_subgraphs" =
  let amatrix = [|
    [| 0.0;  1.0;  0.0;  0.0;  1.0;  0.0 |];
    [| 1.0;  0.0;  1.0;  0.0;  1.0;  0.0 |];
    [| 0.0;  1.0;  0.0;  1.0;  0.0;  0.0 |];
    [| 0.0;  0.0;  1.0;  0.0;  1.0;  1.0 |];
    [| 1.0;  1.0;  0.0;  1.0;  0.0;  0.0 |];
    [| 0.0;  0.0;  0.0;  1.0;  0.0;  0.0 |]
  |] in
  let lmatrix = get_laplacian_matrix amatrix in
  get_num_disjoint_subgraphs ~threshold:1e-14 lmatrix
  |> printf "%d";
  [%expect {| 1 |}]

(**
  Accepts a symmetric matrix and returns the set of eigenvalues and
  eigenvectors in an array of pairs sorted so that the smallest eigenvalues
  are first.
*)
let get_sorted_eigenvectors matrix = 
  let open Eigen.Symmetric in
  let { values; vectors } = get_eigenvectors matrix in
  let res = Array.zip_exn values vectors in
  Array.sort res ~compare:(fun (x, _) (y, _) -> [%compare: float] x y);
  res

(**
  Accepts a graph's Laplacian matrix and divides the graph's nodes into
  three subgroups, those for which the corresponding Fiedler vector value
  is effectively zero, less than zero, and greater than zero.

  Note: You can use the [threshold] value to classify any node whose Fiedler
  value's magnitude is less than or equal to [threshold] as effectively 0.
*)
let get_fiedler_partition ?(threshold = 0.0) lmatrix =
  let evecs = get_sorted_eigenvectors lmatrix in
  if Array.length evecs < 2
  then failwiths ~here:[%here] "[get_fiedler_partition] Error: the laplacian matrix must have at least two eigenvectors." () [%sexp_of: unit]
  else
    evecs.(1) (* the Fiedler vector *)
    |> snd
    |> Array.foldi ~init:([], [], []) ~f:(fun i (lt, eq, gt) x ->
      let open Float in
      match () with
      | () when abs x <= threshold -> (lt, List.cons i eq, gt)
      | () when x < 0.0 -> (List.cons i lt, eq, gt)
      | () when 0.0 < x -> (lt, eq, List.cons i gt)
      | _ -> assert false
    )

let%expect_test "get_fiedler_partition" =
  let amatrix = [|
    [| 0.0;  1.0;  0.0;  0.0;  1.0;  0.0 |];
    [| 1.0;  0.0;  1.0;  0.0;  1.0;  0.0 |];
    [| 0.0;  1.0;  0.0;  1.0;  0.0;  0.0 |];
    [| 0.0;  0.0;  1.0;  0.0;  1.0;  1.0 |];
    [| 1.0;  1.0;  0.0;  1.0;  0.0;  0.0 |];
    [| 0.0;  0.0;  0.0;  1.0;  0.0;  0.0 |]
  |] in
  let lmatrix = get_laplacian_matrix amatrix in
  get_fiedler_partition ~threshold:1e-1 lmatrix
  |> printf !"%{sexp: int list * int list * int list}";
  [%expect {| ((5 3) (2) (4 1 0)) |}]

(**
  Accepts a graph's Laplacian matrix and returns the graph's
  "eigen-projection".

  This function assigns the nodes within a graph to points in R^[dim]. The
  point assignment minimizes the following energy function:

  {% \Sum_{i,j \in Edges} w_{i,j} (p_i - p_j)^2 %}

  where {%  p_i %} is the point that that i-th node is assigned to and {%
  w_{i,j} %} is the weight of the edge connecting nodes i and j.

  Additionally, the point assignment satisfies some additional constraints. The
  variance of the points along each axis equals 1/n (the number of nodes) and
  mean/center of the points equals 0.

  References:
  1. Koren, Yehuda. "On Spectral Graph Drawing." (Retreived 2025).
    https://www.mathe2.uni-bayreuth.de/axel/papers/
    koren:on_spectral_graph_drawing.pdf
*)
let get_eigenprojection ?(dim = 1) lmatrix =
  let evecs = get_sorted_eigenvectors lmatrix |> Array.map ~f:snd in
  let num_evecs = Array.length evecs in
  printf !"evecs: %{sexp: Linalg.vector array}\n" evecs;
  Array.init (Array.length lmatrix) ~f:(fun i ->
    Array.init dim ~f:(fun j ->
      if dim >= num_evecs then failwiths ~here:[%here] "[get_projection] Error: the projection dimension must be less than the number of eigenvectors minus one." () [%sexp_of: unit];
      evecs.(j + 1).(i)
    )
  )

let%expect_test "get_eigenprojection" =
  let amatrix = [|
    [| 0.0;  1.0;  0.0;  0.0;  1.0;  0.0 |];
    [| 1.0;  0.0;  1.0;  0.0;  1.0;  0.0 |];
    [| 0.0;  1.0;  0.0;  1.0;  0.0;  0.0 |];
    [| 0.0;  0.0;  1.0;  0.0;  1.0;  1.0 |];
    [| 1.0;  1.0;  0.0;  1.0;  0.0;  0.0 |];
    [| 0.0;  0.0;  0.0;  1.0;  0.0;  0.0 |]
  |] in
  let num_nodes = Array.length amatrix in
  let lmatrix = get_laplacian_matrix amatrix in
  let ps = get_eigenprojection ~dim:3 lmatrix in
  printf !"%{sexp: Linalg.vector array}\n" ps;
  (* center of the points is 0 *)
  Linalg.(get_mean_vector ps |> Option.value_map ~default:1.0 ~f:vector_norm) |> printf "%f\n";
  (* the variance along each axis equals 1/n *)
  matrix_mult (matrix_transpose ps) ps |> printf !"%{sexp: Linalg.matrix}\n";
  Array.sum (module Float) ps ~f:(fun p -> Float.square p.(0)) /. float num_nodes |> printf "%f\n";
  Array.sum (module Float) ps ~f:(fun p -> Float.square p.(1)) /. float num_nodes |> printf "%f\n";
  Array.sum (module Float) ps ~f:(fun p -> Float.square p.(2)) /. float num_nodes |> printf "%f\n";
  [%expect {||}]
