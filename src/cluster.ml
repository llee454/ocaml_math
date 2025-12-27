(**
  This module defines a collection of functions for clustering points. It
  implements naive k-means and another clustering algorithm based on simulated
  annealing.
*)
open Core
open Stats
open Linalg
open Simulated_annealing

(**
  Accepts a set of points and returns the greatest distance between them.

  Note: to prove that this is correct use induction with the triangle
  inequality.
*)
let get_diameter (points : vector array) = 
  Array.fold points ~init:(0.0, []) ~f:(fun acc p ->
    let open Float in
    match acc with
    | (dist, []) -> (dist, [p])
    | (_, [q]) -> (distance p q, [p; q])
    | (dist, [q; r]) -> begin
      let q_dist = distance p q
      and r_dist = distance p r
      in
      match () with
      | () when (q_dist >= r_dist) && (q_dist >= dist) -> (q_dist, [p; q])
      | () when (r_dist >= q_dist) && (r_dist >= dist) -> (r_dist, [p; r])
      | () -> acc
      end
    | _ -> failwiths ~here:[%here] "Error" () [%sexp_of: unit] 
    )
  |> fst

let%expect_test "get_diameter" =
  let points = Array.init 5_000 ~f:(fun _ -> create_random_vector_lt ~len:10.0 3) in
  printf "%0.2f" @@ get_diameter points;
  [%expect {| 19.90 |}]

let%expect_test "get_diameter" =
  let points =
    [|
      [|0.0; 0.0|];
      [|1.0; 1.0|];
      [|2.0; 0.0|]
    |]
  in get_diameter points |> printf "%0.2f";
  [%expect {| 2.00 |}]

(**
  Accepts a list of points and a cluster and returns the sum of the
  square distances.
*)
let get_sum_sqr_dists points cluster =
  let sum = ref 0.0 in
  for i = 0 to Array.length points - 1 do
    sum := !sum +. Float.square (distance points.(i) cluster) 
  done;
  !sum

(**
  Accepts two arguments [clusters] and [point] and returns the index of and
  distance to the cluster that is closest to the given point. If [clusters]
  is empty, this function returns None.
*)
let get_closest_cluster_index_dist (clusters : vector array) (point : vector) =
  let acc_cluster_ref = ref None
  and acc_dist_ref = ref None
  in for i = 0 to Array.length clusters - 1 do
    let curr = clusters.(i) in
    let curr_dist = distance point curr in
    match !acc_dist_ref with
    | None ->
      acc_cluster_ref := Some i;
      acc_dist_ref := Some curr_dist
    | Some acc_dist when Float.(curr_dist <= acc_dist) ->
      acc_cluster_ref := Some i;
      acc_dist_ref := Some curr_dist
    | _ -> ()
  done;
  match !acc_dist_ref with
  | None -> None
  | Some acc_dist -> Some (Option.value_exn !acc_cluster_ref, acc_dist)
  [@@inline]

let%expect_test "get_closest_cluster_index_dist" =
  let clusters = [|
    [| 0.0; 0.0; 3.0 |];
    [| 0.0; 3.0; 0.0 |];
    [| 3.0; 0.0; 0.0 |];
    [| 1.0; 1.0; 0.0 |]
  |]
  and point = [| 0.0; 0.0; 1.0 |] in
  get_closest_cluster_index_dist clusters point
  |> printf !"%{sexp: (int * float) option}";
  [%expect {| ((3 1.7320508075688772)) |}]

(**
  Accepts a set of points and the centers of a set of clusters and returns
  an array that gives the variance of the distances between the points
  associated with each cluster and the cluster's center.
*)
let get_cluster_variances (points : vector array) (clusters : vector array) =
  let open Float in
  let variances = Array.fold points ~init:(Array.create ~len:(Array.length clusters) None) ~f:(fun acc p ->
    let cluster_data_opt = get_closest_cluster_index_dist clusters p in
    (* update the total variance of the closest cluster *)
    match cluster_data_opt with
    | None -> failwiths ~here:[%here] "Error" () [%sexp_of: unit]
    | Some (cluster_index, cluster_dist) ->
      acc.(cluster_index) <- Some (
        let dist = square (cluster_dist) in
        match acc.(cluster_index) with
        | None -> dist
        | Some cluster_acc -> cluster_acc + dist
      );
      acc
  ) in
  (*
    if a cluster does not have any points associated with it, we measure
    the distance from the cluster and all other points. This encourages
    the algorithm to bring the cluster closer until it grabs a point.
  *)
  Array.mapi variances ~f:(fun i variance_opt ->
    match variance_opt with
    | None ->
      let cluster = clusters.(i) in
      get_sum_sqr_dists points cluster
    | Some variance -> variance)

let create_random_cluster dim center radius = 
  create_random_vector_lt ~len:radius dim |> vector_add center

let create_random_clusters dim center radius =
  Array.init ~f:(fun _ -> create_random_cluster dim center radius)

module Simann = struct
  (** parameters used to tune the simulated annealing algorithm *)
  type t = {
    num_iters: int; (** controls the probability of escaping local minima *)
    num_tries: int; (** controls the probability of escaping local minima *)
    min_temp: float (** controls the precision of convergence *)
  } [@@deriving sexp]

(**
  Accepts a set of normalized points [points] and groups them into
  [num_clusters] clusters.

  Note: this function returns different clusters than k-means - it attempts
  to minimize the mean of the cluster variances.
*)
let get_clusters ?(params = {num_iters = 1_000; num_tries = 10; min_temp = 1e-14}) dim (points : vector array) (num_clusters : Int.t) =
  let radius = get_diameter points /. 2.0 in
  let points_mean = get_mean_vector points |> Option.value_exn ~here:[%here] ~message:"Error: you must give at least one data point." in
  let init_clusters = create_random_clusters dim points_mean radius num_clusters in
  let module M = Simulated_annealing (struct
    type t = vector array
    let copy clusters = Array.copy clusters
    let energy clusters = 
      get_cluster_variances points clusters |> mean
    let step clusters dist =
      let deltas = create_random_vector_lt ~len:dist num_clusters
        |> Array.map ~f:(fun len -> create_random_vector ~len dim)
      in
      Array.mapi clusters ~f:(fun i cluster ->
        vector_add cluster deltas.(i)
      )
    let dist clusters0 clusters1 =
      Array.foldi clusters0 ~init:0.0 ~f:(fun i acc c0 ->
        acc +. Float.square (distance c0 clusters1.(i))
      ) |> Float.sqrt
    let print = None
  end) in
  (*
    the energy needed to jump from the mean to the radius. Initially, we want
    to have a reasonable probability of making jumps at this energy level.
  *)
  let center_jump_energy = get_sum_sqr_dists points points_mean
  and t_min = params.min_temp in
  let gsl_params = M.{
    n_tries = params.num_tries;
    k = 1.0;
    (* we want the initial probability of jumping from the center to the radius
      (assuming that this would zero the energy to be e^-1 *)
    t_initial = center_jump_energy;
    (* we set the cooling rate to bring the initial temperature down to the
      minimum temperature in the given number of steps. *)
    mu_t = exp (-. (1//params.num_iters)*.(log t_min -. log center_jump_energy));
    t_min
  } in
  M.f ~params:gsl_params ~num_iters:params.num_iters ~step_size:radius (M.create_state init_clusters)
end

module K_means = struct
  (**
    Accepts two arguments [clusters] and [point] and returns the index
    of the cluster that is closest to [point] along with the distance between
    the cluster and [point]. If the array of clusters is empty, this function
    returns None.
  *)
  let get_closest_cluster_index clusters point =
    Array.foldi clusters ~init:None ~f:(fun i acc cluster ->
      let dist = distance point cluster in
      match acc with
      | None -> Some (i, dist)
      | Some (_, prev_dist) when Float.(dist <= prev_dist) ->
        Some (i, dist)
      | _ -> acc
    ) |> Option.map ~f:fst

  (**
    Accepts two arguments [points] and [clusters] and, for each cluster,
    returns the center of those points that are closer to it than any other
    cluster. If a cluster is masked, that is, every point is closer to some
    other cluster than it, we return None.
  *)
  let get_means (points : vector array) (clusters : vector array) =
    Array.fold points ~init:(Array.create ~len:(Array.length clusters) None) ~f:(fun acc p ->
      let i = get_closest_cluster_index clusters p |> Option.value_exn ~here:[%here] in
      acc.(i) <- Option.value_map acc.(i) ~default:(Some (1, p)) ~f:(fun (num_points, sum) -> Some (succ num_points, vector_add sum p));
      acc
    ) |>
    Array.map ~f:(Option.map ~f:(fun (num_points, sum) -> vector_scalar_mult (1//num_points) sum))

  (**
    Accepts five arguments [dim], [center], [radius], [points], and [clusters]
    and returns the next set of clusters using the k-means algorithm.

    For each cluster, we find the set of points that are closer to it than
    to any other cluster. We find the location that is in the center of
    these points and return this location as the next location for the cluster.

    If any of the clusters are masked, that is, every point is closer
    to some other cluster than it, we generate a new random location for
    the cluster. This point will be within [radius] distance of the point
    [center]. [radius] should be half of the diameter of the point set and
    [center] should be its center.
  *)
  let get_next dim center radius points clusters =
    get_means points clusters
    |> Array.map ~f:(fun mean -> Option.value mean ~default:(create_random_cluster dim center radius))

  (**
    Accepts three arguments [dim], [points], and [num_clusters], and returns
    [num_clusters] clusters using the k-means algorithm.

    WARNING: if the number of data points is less than the number of clusters,
    this function may never converge/terminate.
  *)
  let get_clusters dim points num_clusters =
    if Array.length points < num_clusters
    then failwiths ~here:[%here] "Error: you must have more points than clusters to run the naive k-means algorithm." () [%sexp_of: unit];
    let radius = get_diameter points /. 2.0 in
    let center = get_mean_vector points |> Option.value_exn ~here:[%here] in
    let init = ref @@ Array.init num_clusters ~f:(fun _ -> create_random_cluster dim center radius) in
    let converged = ref false in
    while not !converged do
      let next = get_next dim center radius points !init in
      converged := [%equal: vector array] !init next;
      init := next
    done;
    !init
end
