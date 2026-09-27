(**
  This module defines functions for working with orthogonal functions. In
  particular, it defines functions that can be used to project a function
  onto an orthogonal bases such as Legendre, Hermite, Laguerre, and Chebyshev
  polynomials.
*)
open Core
open! Float
open! Basic

module Range = struct
  type t = {
    lower: float;
    upper: float;
    singularities: float array;
  }
end

let inner_product Range.{ lower; upper; singularities } f g w =
  let res = Integrate.qagp () ~lower ~upper ~singularities ~f:(fun x -> f x *. g x *. w x) in
  res.out

let length range f w = sqrt @@ inner_product range f f w

let projection range bases f w = Array.map bases ~f:(fun u -> inner_product range f u w /. length range u w)

module Fourier_series = struct
  (**
    Accepts two arguments: [range] and [n]; and returns the basis functions
    for computing a fourier series approximation over the given range using
    [n+1] terms.
  *)
  let bases range n =
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

module Probabilist_Hermite_series = struct

  let weight x = Stats.pdf_normal ~mean:0.0 ~std:1.0 x  

  (** Accepts a number [n] and returns the nth Hermite number [He_n (0)]. *)
  let hermite_num n =
    cos(pi*(n//2))*(Bigint.to_float @@ fact_down_to () ~upper:n ~lower:Int.(n/2))/
    (int_pow (sqrt 2.0) n)

  (**
    Accepts two arguments: [n] and [x]; and returns the value returned by
    the n-th Hermite polynomial for the argument [x]: [He_n (x)].
  *)
  let hermite_poly n x =
    isum () ~init:0 ~max:n ~f:(fun m ->
      (Bigint.to_float @@ binom_coeff () ~n ~k:m)*
      (hermite_num Int.(n - m))*
      (int_pow x m)
    )

  let%expect_test "hermite_poly" =
    [
      1, 1, fact 1;
      2, 2, fact 2;
      3, 3, fact 3;
      2, 3, 0.0
    ]    
    |> List.for_all ~f:(fun (n, m, soln) ->
      let res = (Integrate.qagi () ~f:(fun x ->
        (hermite_poly n x) *
        (hermite_poly m x) *
        weight x
      )).out
      in
      abs (res - soln) < 1e-10
    ) |>
    printf "%b";
    [%expect {| true |}]

  let%expect_test "hermite_poly" =
    let n = 3
    and mean = 1.3 in
    1E-10 > abs
      (Integrate.qagi () ~f:(fun x -> hermite_poly n x * Stats.pdf_normal ~mean ~std:1.0 x)).out -
      (int_pow mean n)
    |> printf "%b";
    [%expect {| true |}]

  (**
    Accepts [num] and returns the first [num] probabilist hermite polynomials: He_n (x).
  *)
  let bases = Sequence.init ~f:hermite_poly

  let approx_normal ?(prec = 100) ~mean ~std =
    let open Float in
    let xs =
      bases prec
      |> Sequence.mapi ~f:(fun n he_n ->
        let kn =
          (cos(pi*(n//2)))/
          (std*(int_pow 2.0 Int.(n + 1))*(fact Int.(n/2))*sqrt_pi)
        in
        kn, he_n
      )
    in
    fun x ->
      let y = (x - mean)/std in
      xs |> Sequence.sum (module Float) ~f:(fun (kn, he_n) -> kn*(he_n y))

  let%expect_test "bases" =
    let x = 0.2
    and mean = 0.1
    and std = 1.1 in
    printf !"%{sexp: float}" @@ abs (approx_normal ~mean ~std x - Stats.pdf_normal ~mean ~std x);
    [%expect {||}]


  let proj n f =
    let open Integrate in
    bases n
    |> Sequence.map ~f:(fun g ->
      (qagi () ~f:(fun x ->
        f (x) * g(x) * (weight x)
      )).out, g
    )
  
  let approx ks x =
    Sequence.sum (module Float) ks ~f:(fun (k, f) ->
      k *. f x
    )
end
