(**
  The Lazy_list module represents lazy lists. Lazy lists are lists in which
  the continuation of the list is deferred.

  They are similar to the streams defined by the [Seq] module which uses thunks
  to represent deferred computations. One of the benefits to using Lazy_lists
  is the ability to use lazy pattern matching to deconstruct these lists.
*)
open! Core
open! Float

module Lazy_list = struct
  type 'a u =
    | Nil : 'a u
    | Cons : 'a * 'a u Lazy.t -> 'a u

  type 'a t = 'a u Lazy.t

  let rec to_list : 'a t -> 'a list = function
  | lazy (Nil) -> []
  | lazy (Cons (x, xs)) -> x :: to_list xs

  let rec of_list : 'a list -> 'a t = function
  | [] -> lazy Nil
  | (x :: xs) -> lazy (Cons (x, of_list xs))

  let%expect_test "to_list/of_list" = 
    printf !"%{sexp: int list}" (to_list (of_list [1; 2; 3]));
    [%expect {| (1 2 3) |}]

  let cons x xs = lazy (Cons (x, xs))

  let cdr = function
  | lazy (Nil) -> None
  | lazy (Cons (_, xs)) -> Some xs

  let empty = lazy Nil

  let rec take n : 'a t -> 'a t = function
  | lazy (Nil) -> lazy (Nil)
  | lazy (Cons (x, xs)) ->
    if Int.(n <= 0)
    then lazy (Nil)
    else lazy (Cons (x, take Int.(n - 1) xs))

  let rec iterate ~f init : 'a t = lazy (Cons (init, iterate ~f (f init)))

  let%expect_test "take/iterate" =
    printf !"%{sexp: int list}" (to_list (take 5 (iterate ~f:Int.succ 0)));
    [%expect {| (0 1 2 3 4) |}]

  (**
    Accepts two arguments, [f] and [init], and returns the sequence:
    [[init, f 1 init, f 2 (f 1 init), ...]]
  *)
  let rec iteratei ~f init : 'a t = iteratei_aux 1 ~f init
  and iteratei_aux i ~f init = lazy (Cons (init, iteratei_aux Int.(i + 1) ~f (f i init)))

  let%expect_test "iteratei" =
    printf !"%{sexp: int list}" (to_list (take 5 (iteratei ~f:(fun i _acc -> i) 0)));
    [%expect {| (0 1 2 3 4) |}]

  let ints : int t = iterate ~f:Int.succ 0

  let%expect_test "ints" =
    printf !"%{sexp: int list}" (to_list (take 5 ints));
    [%expect {| (0 1 2 3 4) |}]

  let uncons : 'a t -> ('a * 'a t) option = function
  | lazy (Nil) -> None
  | lazy (Cons (x, xs)) -> Some (x, xs)

  let rec map ~f : 'a t -> 'b t = function
  | lazy (Cons (x, xs)) -> lazy (Cons (f x, map ~f xs))
  | lazy (Nil) -> lazy (Nil)

  let%expect_test "map" =
    printf !"%{sexp: int list}" (to_list (take 5 (map ~f:(Int.( * ) 3) ints)));
    [%expect {| (0 3 6 9 12) |}]

  let rec mapi ~f xs : 'a t = mapi_aux 0 ~f xs
  and mapi_aux i ~f = function
  | lazy (Nil) -> lazy (Nil)
  | lazy (Cons (x, xs)) -> lazy (Cons (f i x, mapi_aux Int.(i + 1) ~f xs))

  let%expect_test "mapi" =
    printf !"%{sexp: int list}" (to_list (take 5 (mapi ~f:(fun i x -> Int.(i * x)) ints)));
    [%expect {| (0 1 4 9 16) |}]

  let rec map_pairs ~f = function
  | lazy (Cons (x0, (lazy (Cons (x1, _)) as xs))) -> lazy (Cons (f x0 x1, map_pairs ~f xs))
  | lazy (_) -> lazy Nil
end
