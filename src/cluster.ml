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

(**
  Accepts a list of points and a cluster and returns the sum of the
  square distances.
*)
let get_sum_sqr_dists points cluster =
  Array.sum (module Float) points ~f:(fun point ->
    Float.square (distance point cluster)
  )

(**
  Accepts a set of points and the centers of a set of clusters and returns
  an array that gives the variance of the distances between the points
  associated with each cluster and the cluster's center.
*)
let get_cluster_variances (points : vector array) (clusters : vector array) =
  let open Float in
  let variances = Array.fold points ~init:(Array.create ~len:(Array.length clusters) None) ~f:(fun acc p ->
    (* identify the closest cluster *)
    let cluster_data_opt = Array.foldi clusters ~init:None ~f:(fun i prev_opt curr ->
      let curr_dist = distance p curr in
      match prev_opt with
      | None -> Some (i, curr, curr_dist)
      | Some (_prev_index, _prev_center, prev_dist) ->
        if curr_dist <= prev_dist
        then Some (i, curr, curr_dist)
        else prev_opt
    ) in 
    (* update the total variance of the closest cluster *)
    match cluster_data_opt with
    | None -> failwiths ~here:[%here] "Error" () [%sexp_of: unit]
    | Some (cluster_index, _cluster_center, cluster_dist) ->
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

(**
  Accepts a set of normalized points [points] and groups them into
  [num_clusters] clusters.

  By normalized, we mean that the center of the points is the 0 vector
  and the variance is 1.
*)
let get_clusters ?(num_iters = 1_000) dim (points : vector array) (num_clusters : Int.t) =
  let radius = get_diameter points /. 2.0 in
  let points_mean = get_mean_vector points |> Option.value_exn ~here:[%here] ~message:"Error: you must give at least one data point." in
  let init_clusters = Array.init num_clusters ~f:(fun _ -> create_random_vector ~len:radius dim |> vector_add points_mean) in
  let module M = Simulated_annealing (struct
    type t = vector array
    let copy clusters = Array.copy clusters
    let energy clusters = 
      get_cluster_variances points clusters |> mean
    let step clusters dist =
      let deltas = create_random_vector ~len:dist num_clusters
        |> Array.map ~f:(fun len -> create_random_vector ~len 3)
      in
      Array.mapi clusters ~f:(fun i cluster ->
        vector_add cluster deltas.(i)
      )
    let dist clusters0 cluster1 =
      Array.foldi clusters0 ~init:0.0 ~f:(fun i acc c0 ->
        acc +. Float.square (distance c0 cluster1.(i))
      ) |> Float.sqrt
    let print = None
  end) in
  M.f ~num_iters ~step_size:radius (M.create_state init_clusters)
