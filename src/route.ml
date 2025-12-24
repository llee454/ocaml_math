open Core

module Point = struct
  type 'a t = {
    label: 'a;
    lat: float;
    lon: float;
  }
  [@@deriving equal, sexp]

  let dist p q =
    let open Float in
    sqrt (square (p.lat - q.lat) + square (p.lon - q.lon))
end

(**
  Accepts two arguments: [start], a point; and [points], a list of
  points; and returns the shortest path that starts with [start]
  and passes through all of the points in [points].

  WARNING: this function uses exhaustive depth-first search
  and does not scale past a few points. Note however that this
  function will return the absolute shortest path if it exists.

  Example:

  {[
  let len, path =
    find_path
      ~label_equal:String.equal
      Point.{ label = "3804 Delverne Rd, Baltimore, MD 21218"; lat = 39.33428; lon = -76.59854 }
      Point.
        [
          { label = "1517 Tunlaw Rd, Baltimore, MD 21218"; lat = 39.33511; lon = -76.59328 };
          { label = "3809 Monterey Rd, Baltimore, MD 21218"; lat = 39.33438; lon = -76.59716 };
          { label = "1527 Tunlaw Rd, Baltimore, MD 21218"; lat = 39.33512; lon = -76.59287 };
          { label = "3814 The Alameda, Baltimore, MD 21218"; lat = 39.33499; lon = -76.59688 };
          { label = "1621 Ralworth Rd, Baltimore, MD 21218"; lat = 39.3392; lon = -76.59058 };
          { label = "1533 Medford Rd, Baltimore, MD 21218"; lat = 39.33582; lon = -76.59241 };
          { label = "3912 The Alameda, Baltimore, MD 21218"; lat = 39.33665; lon = -76.59888}
        ]
  in ...
  ]}
*)
let find_path ~label_equal start points =
  let rec aux curr_len last_point curr_path =
    List.fold points ~init:None ~f:(fun acc point ->
        if List.mem curr_path point ~equal:(Point.equal label_equal)
        then acc
        else (
          let ((new_len, _) as new_res) =
            aux Float.(curr_len + Point.dist last_point point) point (point :: curr_path)
          in
          match acc with
          | None -> Some new_res
          | Some (other_len, _) when Float.(new_len < other_len) -> Some new_res
          | Some other -> Some other ) )
    |> function
    | None -> curr_len, curr_path
    | Some res -> res
  in
  let len, path = aux 0.0 start (List.singleton start) in
  len, List.rev path
