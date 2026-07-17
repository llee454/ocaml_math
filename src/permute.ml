(**
  This module defines useful functions for calculating permutations.
*)
open Core
open Basic

(**
  Accepts a random finite sequence of values [xs] and returns a permutation
  of them.

  Note that every permutation returned by this function is drawn from a
  uniform distribution - i.e. every permutation has an equal probability
  of being generated.

  Note this function uses the [Random] module. Use [Random.init] and other
  functions to initialize the random number generator.
*)
let f xs =
  if Sequence.is_empty xs
  then Sequence.empty
  else
    let len = Sequence.length xs in
    let tree = Fillable_vector.create ~f:(Fn.const None) ~is_full:(fun _i _x -> false) len |> Option.value_exn in
    Sequence.iteri xs ~f:(fun i x ->
      let j = Random.int (len - i) in
      Fillable_vector.update j tree ~unfilled_only:true ~f:(Fn.const (Some x))
        ~is_full:(Fn.const true)
    );
    Fillable_vector.get_leaves tree
    |> Sequence.map ~f:(fun x -> Option.value_exn x)

let%expect_test "Permute.f" =
  Sequence.of_list [1; 2; 3; 4; 5]
  |> f 
  |> printf !"%{sexp: int Sequence.t}";
  [%expect {| (2 4 3 5 1) |}]

let%expect_test "Permute.f" =
  (List.init 3 ~f:(Fn.const "A") @
   List.init 5 ~f:(Fn.const "B") @
   List.init 2 ~f:(Fn.const "C"))
  |> Sequence.of_list
  |> f 
  |> printf !"%{sexp: string Sequence.t}";
  [%expect {| (A B B B B C A C B A) |}]

(**
  Accepts two arguments, [num_ones] and [len], and returns a random binary
  sequence of length [len] containing [num_ones] ones.

  Note: this function selects binary sequences from a uniform distribution -
  every possible sequence has the same probability as every other.

  Note: this function relies on the [Random] module. Use [Random.init]
  and other functions to initialize the random number generator.
*)
let gen_rand_binary_seq ~num_ones ~len () =
  if len <= 0
  then Sequence.empty
  else
    if len < num_ones
    then failwiths ~here:[%here] "Error: an error occured while trying to generate a binary sequence with a given number of ones. The number of ones requested was longer than the sequence." (num_ones, len) [%sexp_of: (int * int)]
    else
      let tree = Fillable_vector.create ~f:(Fn.const 0) ~is_full:(fun _i _x -> false) len |> Option.value_exn in
      for i = 0 to num_ones - 1 do
        let j = Random.int (len - i) in
        Fillable_vector.update j tree ~unfilled_only:true ~f:(Fn.const 1) ~is_full:(Fn.const true)
      done;
      Fillable_vector.get_leaves tree

let%expect_test "gen_rand_binary_seq" =
  let seq = gen_rand_binary_seq ~num_ones:0 ~len:0 ()
  |> Sequence.to_list
  in
  let len      = List.length seq
  and num_ones = List.count seq ~f:([%equal: int] 1)
  in
  printf !"%d %d %{sexp: int list}\n" len num_ones seq;
  [%expect {| 0 0 () |}]

let%expect_test "gen_rand_binary_seq" =
  let seq = gen_rand_binary_seq ~num_ones:0 ~len:1 ()
  |> Sequence.to_list
  in
  let len      = List.length seq
  and num_ones = List.count seq ~f:([%equal: int] 1)
  in
  printf !"%d %d %{sexp: int list}\n" len num_ones seq;
  [%expect {| 1 0 (0) |}]

let%expect_test "gen_rand_binary_seq" =
  let seq = gen_rand_binary_seq ~num_ones:1 ~len:1 ()
  |> Sequence.to_list
  in
  let len      = List.length seq
  and num_ones = List.count seq ~f:([%equal: int] 1)
  in
  printf !"%d %d %{sexp: int list}\n" len num_ones seq;
  [%expect {| 1 1 (1) |}]

let%expect_test "gen_rand_binary_seq" =
  let seq = gen_rand_binary_seq ~num_ones:7 ~len:27 ()
  |> Sequence.to_list
  in
  let len      = List.length seq
  and num_ones = List.count seq ~f:([%equal: int] 1)
  in
  printf !"%d %d %{sexp: int list}\n" len num_ones seq;
  [%expect {| 27 7 (0 0 0 1 0 0 1 0 0 0 0 0 0 0 1 0 0 0 1 1 0 0 1 0 1 0 0) |}]

module Perm_value = struct
  (**
    Represents information about a value that should appear in a permuted
    list. Specifically, the number of times the value must appear.
  *)
  type 'a t = {
    label: 'a;
    mutable num: int
  } [@@deriving fields, sexp]

  let copy ~f x = { label = f x.label; num = x.num }
end

module Partial_perm = struct
  (**
    Represents a partial permutation sequence along with a set of remaining
    values that can be used in the continuation.
  *)
  type 'a t = {
    seq: 'a list;
    rem_vals: 'a Perm_value.t Fillable_vector.Tree.t
  } [@@deriving sexp]

  (**
    Acceptsa partial permutation and returns a sequence of the continuations
    were we add a new value, drawn from rem_vals, to the sequence.
  *)
  let extend ~(copy_label : 'a -> 'a) (x : 'a t) : 'a t Sequence.t =
    let n = Fillable_vector.get_num_available x.rem_vals in
    Sequence.init n ~f:(fun index ->
      let i = n - 1 - index in
      let rem_vals = Fillable_vector.Tree.copy ~f:(Perm_value.copy ~f:copy_label) x.rem_vals in
      let xi = Fillable_vector.get_nth_unfilled i rem_vals
        |> Option.value_exn ~here:[%here]
      in
      Fillable_vector.update ~unfilled_only:true
        ~f:(fun info -> 
          let open Perm_value in
          info.num <- info.num - 1;
          info)
        ~is_full:(fun info -> [%equal: int] info.num 0)
        i rem_vals;
      { rem_vals; seq = List.cons xi.label x.seq }
    )
end

(**
  Accepts an array of values and returns a fillable vector that stores
  these values
*)
let fv_of_array ~(copy_label : 'a -> 'a) (vals : 'a Perm_value.t array) =
  Fillable_vector.create
    ~f:(fun i -> Perm_value.copy vals.(i) ~f:copy_label)
    ~is_full:(fun _i rem_val -> rem_val.num = 0)
    (Array.length vals)

(**
  Accepts a fillable vector and updates that nth slot using the given function.

  Note: the index counts all leaves not just the unfilled slots.
*)
let update_nth_val ~f nth : 'a Perm_value.t Fillable_vector.Tree.t -> unit =
  Fillable_vector.update
    ~unfilled_only:false
    ~f:(fun (rem_val : 'a Perm_value.t) ->
      rem_val.num <- f rem_val.num;
      rem_val
    )
    ~is_full:(fun (rem_val : 'a Perm_value.t) -> rem_val.num = 0)
    nth

let incr_nth_val nth vals = update_nth_val ~f:Int.succ nth vals

let decr_nth_val nth vals = update_nth_val ~f:(fun num -> num - 1) nth vals

(**
  Accepts a set of values and returns the number of permutations that can
  be formed using them.
*)
let get_num_perms (values : 'a Perm_value.t array) : float =
  fact (Array.sum (module Int) values ~f:(fun value -> value.num)) /.
  Array.fold values ~init:1.0 ~f:(fun acc value ->
    acc *. (fact @@ Perm_value.num value)
  )

(**
  Accepts a set of values and returns the set of all permutations of them.
*)
let get_all_permutations_eager ~copy_label (values : 'a Perm_value.t array) =
  let num_vals = Array.sum (module Int) ~f:Perm_value.num values in
  if num_vals = 0
  then Sequence.singleton []
  else begin
    let perms = ref @@ Sequence.singleton
      Partial_perm.{
        rem_vals = fv_of_array ~copy_label values |> Option.value_exn ~here:[%here];
        seq = []
      }
    in
    for _i = 0 to num_vals - 1 do
      perms := Sequence.concat_map !perms ~f:(fun perm -> Partial_perm.extend ~copy_label perm)
    done;
    Sequence.map ~f:(fun (x : 'a Partial_perm.t) -> x.seq) !perms
  end

let%expect_test "get_all_permutation_vecs" =
  let copy_label = String.of_string
  and values = [| |]
  in
  let num_perms = Int.of_float @@ get_num_perms values in
  let perms = get_all_permutations_eager ~copy_label values |> Sequence.to_array in
  printf !"%b\n %d\n %{sexp: string list array}\n" (num_perms = Array.length perms) num_perms perms;
  [%expect {|
    true
     1
     (())
    |}]

let%expect_test "get_all_permutation_vecs" =
  let copy_label = String.of_string
  and values = [|
      Perm_value.{ label = "A"; num = 0 };
    |]
  in
  let num_perms = Int.of_float @@ get_num_perms values in
  let perms = get_all_permutations_eager ~copy_label values |> Sequence.to_array in
  printf !"%b\n %d\n %{sexp: string list array}\n" (num_perms = Array.length perms) num_perms perms;
  [%expect {|
    true
     1
     (())
    |}]

let%expect_test "get_all_permutation_vecs" =
  let copy_label = String.of_string
  and values = [|
      Perm_value.{ label = "A"; num = 0 };
      Perm_value.{ label = "B"; num = 0 };
    |]
  in
  let num_perms = Int.of_float @@ get_num_perms values in
  let perms = get_all_permutations_eager ~copy_label values |> Sequence.to_array in
  printf !"%b\n %d\n %{sexp: string list array}\n" (num_perms = Array.length perms) num_perms perms;
  [%expect {|
    true
     1
     (())
    |}]

let%expect_test "get_all_permutation_vecs" =
  let copy_label = String.of_string
  and values = [|
      Perm_value.{ label = "A"; num = 1 };
      Perm_value.{ label = "B"; num = 0 };
    |]
  in
  let num_perms = Int.of_float @@ get_num_perms values in
  let perms = get_all_permutations_eager ~copy_label values |> Sequence.to_array in
  printf !"%b\n %d\n %{sexp: string list array}\n" (num_perms = Array.length perms) num_perms perms;
  [%expect {|
    true
     1
     ((A))
    |}]

let%expect_test "get_all_permutation_vecs" =
  let copy_label = String.of_string
  and values = [|
      Perm_value.{ label = "A"; num = 1 };
      Perm_value.{ label = "B"; num = 1 };
      Perm_value.{ label = "C"; num = 1 }
    |]
  in
  let num_perms = Int.of_float @@ get_num_perms values in
  let perms = get_all_permutations_eager ~copy_label values |> Sequence.to_array in
  printf !"%b\n %d\n %{sexp: string list array}\n" (num_perms = Array.length perms) num_perms perms;
  [%expect {|
    true
     6
     ((A B C) (B A C) (A C B) (C A B) (B C A) (C B A))
    |}]

let%expect_test "get_all_permutation_vecs" =
  let copy_label = String.of_string
  and values = [|
      Perm_value.{ label = "A"; num = 1 };
      Perm_value.{ label = "B"; num = 2 };
      Perm_value.{ label = "C"; num = 1 }
    |]
  in
  let num_perms = Int.of_float @@ get_num_perms values in
  let perms = get_all_permutations_eager ~copy_label values |> Sequence.to_array in
  printf !"%b\n %d\n %{sexp: string list array}\n" (num_perms = Array.length perms) num_perms perms;
  [%expect {|
    true
     12
     ((A B B C) (B A B C) (B B A C) (A B C B) (B A C B) (A C B B) (C A B B)
     (B C A B) (C B A B) (B B C A) (B C B A) (C B B A))
    |}]

let%expect_test "get_all_permutation_vecs" =
  let copy_label = String.of_string
  and values = [|
      Perm_value.{ label = "A"; num = 3 };
      Perm_value.{ label = "B"; num = 1 };
      Perm_value.{ label = "C"; num = 1 }
    |]
  in
  let num_perms = Int.of_float @@ get_num_perms values in
  let perms = get_all_permutations_eager ~copy_label values |> Sequence.to_array in
  printf !"%b\n %d\n %{sexp: string list array}\n" (num_perms = Array.length perms) num_perms perms;
  [%expect {|
    true
     20
     ((A A A B C) (A A B A C) (A B A A C) (B A A A C) (A A A C B) (A A C A B)
     (A C A A B) (C A A A B) (A A B C A) (A B A C A) (B A A C A) (A A C B A)
     (A C A B A) (C A A B A) (A B C A A) (B A C A A) (A C B A A) (C A B A A)
     (B C A A A) (C B A A A))
    |}]

let%expect_test "get_all_permutation_vecs" =
  let copy_label = String.of_string
  and values = [|
      Perm_value.{ label = "A"; num = 3 };
      Perm_value.{ label = "B"; num = 5 };
      Perm_value.{ label = "C"; num = 2 }
    |]
  in
  let num_perms = Int.of_float @@ get_num_perms values in
  let perms = get_all_permutations_eager ~copy_label values in
  printf !"%b %d\n" (num_perms = Sequence.length perms) num_perms;
  [%expect {| true 2520 |}]

let%expect_test "get_all_permutation_vecs" =
  let copy_label = String.of_string
  and values = [|
      Perm_value.{ label = "A"; num = 3 };
      Perm_value.{ label = "B"; num = 5 };
      Perm_value.{ label = "C"; num = 2 };
      Perm_value.{ label = "D"; num = 0 }
    |]
  in
  let num_perms = Int.of_float @@ get_num_perms values in
  let perms = get_all_permutations_eager ~copy_label values in
  printf !"%b %d\n" (num_perms = Sequence.length perms) num_perms;
  [%expect {| true 2520 |}]

(**
  Accepts a list of partial permutations and returns the full permutations
  in a lazy list.
*)
let rec get_all_permutations_aux ~(copy_label : 'a -> 'a) (partial_perms : 'a Partial_perm.t Queue.t) : 'a list Lazy_list.t =
  match Queue.dequeue_back partial_perms with
  | None -> Lazy_list.empty
  | Some partial_perm ->
    let n = Fillable_vector.get_num_available partial_perm.rem_vals in
    if n = 0
    then lazy (Lazy_list.Cons (partial_perm.seq, get_all_permutations_aux ~copy_label partial_perms))
    else begin
      for i = 0 to n - 1 do
        let next_rem_vals = Fillable_vector.Tree.copy partial_perm.rem_vals ~f:(Perm_value.copy ~f:copy_label) in
        let next_val_lbl = (Option.value_exn ~here:[%here] @@ Fillable_vector.get_nth_unfilled i next_rem_vals).label in
        Fillable_vector.update ~unfilled_only:true
          ~f:(fun (rem_val : 'a Perm_value.t) ->
            rem_val.num <- rem_val.num - 1;
            rem_val
          )
          ~is_full:(fun (rem_val : 'a Perm_value.t) -> rem_val.num = 0)
          i next_rem_vals;
        Queue.enqueue partial_perms Partial_perm.{
          seq = List.cons next_val_lbl partial_perm.seq;
          rem_vals = next_rem_vals
        };
      done;
      get_all_permutations_aux ~copy_label partial_perms
    end

(**
  Accepts two arguments:

  * copy_label, a function that accepts a label and returns a copy of it
  * and rem_vals, an array that lists a set of values and the number of
    times each of them can appear in a sequence

  and returns every sequence in which the given values appear the given
  number of times as a lazy list.
*)
let get_all_permutations ~(copy_label : 'a -> 'a) (rem_vals : 'a Perm_value.t array) : 'a list Lazy_list.t =
  let rem_vals_opt = fv_of_array ~copy_label rem_vals in
  match rem_vals_opt with
  | None -> Lazy_list.cons [] Lazy_list.empty
  | Some rem_vals ->
    get_all_permutations_aux ~copy_label @@ Queue.singleton Partial_perm.{
      seq = [];
      rem_vals
    }

let%expect_test "get_all_permutations" =
  let copy_label = String.of_string
  and values = [| |]
  in
  let num_perms = Int.of_float @@ get_num_perms values in
  let perms = get_all_permutations ~copy_label values |> Lazy_list.to_list in
  printf !"%b\n %d\n %{sexp: string list list}\n" (num_perms = List.length perms) num_perms perms;
  [%expect {|
    true
     1
     (())
    |}]

let%expect_test "get_all_permutations" =
  let copy_label = String.of_string
  and values = [|
      Perm_value.{ label = "A"; num = 0 };
    |]
  in
  let num_perms = Int.of_float @@ get_num_perms values in
  let perms = get_all_permutations ~copy_label values |> Lazy_list.to_list in
  printf !"%b\n %d\n %{sexp: string list list}\n" (num_perms = List.length perms) num_perms perms;
  [%expect {|
    true
     1
     (())
    |}]

let%expect_test "get_all_permutations" =
  let copy_label = String.of_string
  and values = [|
      Perm_value.{ label = "A"; num = 0 };
      Perm_value.{ label = "B"; num = 0 };
    |]
  in
  let num_perms = Int.of_float @@ get_num_perms values in
  let perms = get_all_permutations ~copy_label values |> Lazy_list.to_list in
  printf !"%b\n %d\n %{sexp: string list list}\n" (num_perms = List.length perms) num_perms perms;
  [%expect {|
    true
     1
     (())
    |}]

let%expect_test "get_all_permutations" =
  let copy_label = String.of_string
  and values = [|
      Perm_value.{ label = "A"; num = 1 };
      Perm_value.{ label = "B"; num = 0 };
    |]
  in
  let num_perms = Int.of_float @@ get_num_perms values in
  let perms = get_all_permutations ~copy_label values |> Lazy_list.to_list in
  printf !"%b\n %d\n %{sexp: string list list}\n" (num_perms = List.length perms) num_perms perms;
  [%expect {|
    true
     1
     ((A))
    |}]

let%expect_test "get_all_permutations" =
  let copy_label = String.of_string
  and values = [|
      Perm_value.{ label = "A"; num = 1 };
      Perm_value.{ label = "B"; num = 1 };
      Perm_value.{ label = "C"; num = 1 }
    |]
  in
  let num_perms = Int.of_float @@ get_num_perms values in
  let perms = get_all_permutations ~copy_label values |> Lazy_list.to_list in
  printf !"%b\n %d\n %{sexp: string list list}\n" (num_perms = List.length perms) num_perms perms;
  [%expect {|
    true
     6
     ((A B C) (B A C) (A C B) (C A B) (B C A) (C B A))
    |}]

let%expect_test "get_all_permutations" =
  let copy_label = String.of_string
  and values = [|
      Perm_value.{ label = "A"; num = 1 };
      Perm_value.{ label = "B"; num = 2 };
      Perm_value.{ label = "C"; num = 1 }
    |]
  in
  let num_perms = Int.of_float @@ get_num_perms values in
  let perms = get_all_permutations ~copy_label values |> Lazy_list.to_list in
  printf !"%b\n %d\n %{sexp: string list list}\n" (num_perms = List.length perms) num_perms perms;
  [%expect {|
    true
     12
     ((A B B C) (B A B C) (B B A C) (A B C B) (B A C B) (A C B B) (C A B B)
     (B C A B) (C B A B) (B B C A) (B C B A) (C B B A))
    |}]

let%expect_test "get_all_permutations" =
  let copy_label = String.of_string
  and values = [|
      Perm_value.{ label = "A"; num = 3 };
      Perm_value.{ label = "B"; num = 1 };
      Perm_value.{ label = "C"; num = 1 }
    |]
  in
  let num_perms = Int.of_float @@ get_num_perms values in
  let perms = get_all_permutations ~copy_label values |> Lazy_list.to_list in
  printf !"%b\n %d\n %{sexp: string list list}\n" (num_perms = List.length perms) num_perms perms;
  [%expect {|
    true
     20
     ((A A A B C) (A A B A C) (A B A A C) (B A A A C) (A A A C B) (A A C A B)
     (A C A A B) (C A A A B) (A A B C A) (A B A C A) (B A A C A) (A A C B A)
     (A C A B A) (C A A B A) (A B C A A) (B A C A A) (A C B A A) (C A B A A)
     (B C A A A) (C B A A A))
    |}]

let%expect_test "get_all_permutations" =
  let copy_label = String.of_string
  and values = [|
      Perm_value.{ label = "A"; num = 3 };
      Perm_value.{ label = "B"; num = 5 };
      Perm_value.{ label = "C"; num = 2 }
    |]
  in
  let num_perms = Int.of_float @@ get_num_perms values in
  let perms = get_all_permutations ~copy_label values |> Lazy_list.to_list in
  printf !"%b %d\n" (num_perms = List.length perms) num_perms;
  [%expect {| true 2520 |}]

let%expect_test "get_all_permutations" =
  let copy_label = String.of_string
  and values = [|
      Perm_value.{ label = "A"; num = 3 };
      Perm_value.{ label = "B"; num = 5 };
      Perm_value.{ label = "C"; num = 2 };
      Perm_value.{ label = "D"; num = 0 }
    |]
  in
  let num_perms = Int.of_float @@ get_num_perms values in
  let perms = get_all_permutations ~copy_label values |> Lazy_list.to_list in
  printf !"%b %d\n" (num_perms = List.length perms) num_perms;
  [%expect {| true 2520 |}]

let%expect_test "get_all_permutations" =
  let copy_label = String.of_string
  and values = [|
      Perm_value.{ label = "A"; num = 3 };
      Perm_value.{ label = "B"; num = 5 };
      Perm_value.{ label = "C"; num = 2 };
      Perm_value.{ label = "D"; num = 0 };
    |]
  in
  let num_perms = get_num_perms values in
  let perm = get_all_permutations ~copy_label values |> Lazy_list.nth 1111 in
  printf !"%.0f %{sexp: string list option}\n" num_perms perm;
  [%expect {| 2520 ((B C A B C A A B B B)) |}]

let%expect_test "get_all_permutations" =
  let copy_label = String.of_string
  and values = [|
      Perm_value.{ label = "A"; num = 3 };
      Perm_value.{ label = "B"; num = 5 };
      Perm_value.{ label = "C"; num = 2 };
      Perm_value.{ label = "D"; num = 0 };
      Perm_value.{ label = "E"; num = 3 };
      Perm_value.{ label = "F"; num = 5 };
      Perm_value.{ label = "G"; num = 2 };
      Perm_value.{ label = "H"; num = 0 };
      Perm_value.{ label = "I"; num = 3 };
      Perm_value.{ label = "J"; num = 5 };
      Perm_value.{ label = "K"; num = 2 };
      Perm_value.{ label = "L"; num = 0 };
    |]
  in
  let num_perms = get_num_perms values in
  let perm = get_all_permutations ~copy_label values |> Lazy_list.nth 1_000_000 in
  printf !"%.0f %{sexp: string list option}\n" num_perms perm;
  [%expect {| 88832646059788345540608 ((A B B B E A A C B C F B E E F F F F G G I I I J J J J J K K)) |}]

  (**
    Accepts two arguments: nth; and vals; and returns the nth permutation
    sequence where every value is take from vals and appears the given number
    of times.
  *)
  let get_nth_permutation ~(copy_label : 'a -> 'a) nth (vals : 'a Perm_value.t array) =
    try
    let len = Array.sum (module Int) vals ~f:(fun (rem_val : 'a Perm_value.t) -> rem_val.num) in
    if len = 0 then raise_notrace Exit;
    let seq = Queue.create ~capacity:len ()
    and rem_vals = fv_of_array ~copy_label vals |> Option.value_exn ~here:[%here]
    and num_seqs = ref 0
    and offset = ref nth in
    for _i = 0 to len - 1 do
      let num_rem_vals = Fillable_vector.get_num_available rem_vals in
      let appended_val =
        try 
          for j = num_rem_vals - 1 downto 0 do
            let child_idx = Fillable_vector.get_nth_unfilled_index j rem_vals |> Option.value_exn ~here:[%here] in
            let child_val = Fillable_vector.get_nth child_idx rem_vals in
            decr_nth_val child_idx rem_vals;
            let num_child_seqs = Fillable_vector.get_leaves rem_vals |> Sequence.to_array |> get_num_perms |> Int.of_float in
            let next_num_seqs = !num_seqs + num_child_seqs in
            if !num_seqs <= !offset && !offset < next_num_seqs
            then begin
              Queue.enqueue_front seq (Option.value_exn ~here:[%here] child_val).label;
              offset := !offset - !num_seqs;
              raise_notrace Exit
            end;
            incr_nth_val child_idx rem_vals;
            num_seqs := next_num_seqs
          done;
          false
        with
        | Exit -> true
      in
      if not appended_val then raise_notrace Exit;
      num_seqs := 0
    done;
    Some seq
  with  
  | Exit -> None

  let%expect_test "get_nth_permutation" =
    let copy_label = String.of_string
    and values = [|
        Perm_value.{ label = "A"; num = 3 };
        Perm_value.{ label = "B"; num = 5 };
        Perm_value.{ label = "C"; num = 2 };
        Perm_value.{ label = "D"; num = 0 };
      |]
    in
    let num_perms = get_num_perms values in
    let perm_res = get_nth_permutation ~copy_label 2519 values |> Option.map ~f:Queue.to_array in
    let perm_ref = get_all_permutations ~copy_label values |> Lazy_list.nth 2519 in
    printf !"%.0f %{sexp: string list option} %{sexp: string array option}\n" num_perms perm_ref perm_res;
    [%expect {| 2520 ((C C B B B B B A A A)) ((C C B B B B B A A A)) |}]

  let%expect_test "get_nth_permutation" =
    let copy_label = String.of_string
    and values = [|
        Perm_value.{ label = "A"; num = 3 };
        Perm_value.{ label = "B"; num = 5 };
        Perm_value.{ label = "C"; num = 2 };
        Perm_value.{ label = "D"; num = 0 };
      |]
    in
    let num_perms = get_num_perms values in
    let perm_res = get_nth_permutation ~copy_label 1111 values |> Option.map ~f:Queue.to_array in
    let perm_ref = get_all_permutations ~copy_label values |> Lazy_list.nth 1111 in
    printf !"%.0f %{sexp: string list option} %{sexp: string array option}\n" num_perms perm_ref perm_res;
    [%expect {| 2520 ((B C A B C A A B B B)) ((B C A B C A A B B B)) |}]
