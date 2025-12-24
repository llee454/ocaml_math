(**
  This module defines functions for working with orthogonal functions. In
  particular, it defines functions that can be used to project a function
  onto an orthogonal bases such as Legendre, Hermite, Laguerre, and Chebyshev
  polynomials.
*)
open Core

module Range = struct
  type t = {
    lower: float;
    upper: float;
    singularities: float array;
  }
end

let inner_product Range.{ lower; upper; singularities } f g =
  let res = Integrate.qagp () ~lower ~upper ~singularities ~f:(fun x -> f x *. g x) in
  res.out

let length range f = sqrt @@ inner_product range f f

let projection range bases f = Array.map bases ~f:(fun u -> inner_product range f u /. length range u)

module Fourier_series = struct
  (**
    Accepts two arguments: [range] and [n]; and returns the basis functions
    for computing a fourier series approximation over the given range using
    [n+1] terms.
  *)
  let bases range n =
    let open Float in
    let range_width = Range.(range.upper - range.lower) in
    let range_middle = Range.(range.lower + (range_width / 2.0)) in
    Sequence.append
      (Sequence.singleton (Fn.const 1.0))
      (Sequence.concat
         (Sequence.init n ~f:(fun i ->
              let m = float i + 1.0 in
              Sequence.append
                (Sequence.singleton (fun x -> cos (2.0 * pi * (x - range_middle) * m / range_width)))
                (Sequence.singleton (fun x -> sin (2.0 * pi * (x - range_middle) * m / range_width))) )
         ) )
    |> Sequence.to_array
end
