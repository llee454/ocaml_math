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

(* let%expect_test "gen_rand_binary_seq" =
  let num_seqs = 10 in
  let seq_len = 10
  and num_ones = 3 in
  let get_expectation (xs : int Sequence.t) =
    let open Float in
    let k = 0.5 in
    ((k - 1.0)/(k - pow_int k Int.(- seq_len))) *
    Sequence.foldi xs ~init:0.0 ~f:(fun m acc x ->
      acc + (float x)/(pow_int k m)
    )
  in
  Sequence.init num_seqs ~f:(fun _ -> gen_rand_binary_seq ~num_ones ~len:seq_len ())
  |> Sequence.map ~f:get_expectation
  |> printf !"%{sexp: float Sequence.t}";
  [%expect {||}] *)

(**
  Represents information about a value that should appear in a permuted
  list. Specifically, the number of times the value must appear.
*)
module Perm_value = struct
  type 'a t = {
    label: 'a;
    mutable num: int
  } [@@deriving fields, sexp]

  let copy ~f x = { label = f x.label; num = x.num }
end

(**
  Represents a partial permutation.
*)
module Partial_perm = struct
  type 'a t = {
    rem_vals: 'a Perm_value.t Fillable_vector.Tree.t;
    perm: 'a list;
  } [@@deriving sexp]

  let extend ~(copy_label : 'a -> 'a) (x : 'a t) : 'a t Sequence.t =
    let n = Fillable_vector.get_num_available x.rem_vals in
    Sequence.init n ~f:(fun i ->
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
      { rem_vals; perm = List.cons xi.label x.perm }
    )
end

(**
  Accepts a set of values and returns the number of permutations that can
  be formed using them.
 *)
let get_num_perms (values : 'a Perm_value.t array) : int =
  Int.of_float @@
    fact (Array.sum (module Int) values ~f:(fun value -> value.num)) /.
    Array.fold values ~init:1.0 ~f:(fun acc value ->
      acc *. (fact @@ Perm_value.num value)
    )

(**
  Accepts a set of values and returns the set of all permutations of them.
*)
let get_all_permutation_vecs ~copy_label (values : 'a Perm_value.t array) =
  let num_vals = Array.sum (module Int) ~f:Perm_value.num values in
  if num_vals = 0
  then [| [] |]
  else begin
    let perms = ref @@ Sequence.singleton
      Partial_perm.{
        rem_vals = Option.value_exn ~here:[%here] @@
          Fillable_vector.create (Array.length values)
            ~f:(fun i -> values.(i))
            ~is_full:(fun _i info -> info.num = 0);
        perm = []
      }
    in
    for _i = 0 to num_vals - 1 do
      perms := Sequence.concat_map !perms ~f:(fun perm -> Partial_perm.extend ~copy_label perm)
    done;
    !perms
    |> Sequence.map ~f:(fun (x : 'a Partial_perm.t) -> x.perm)
    |> Sequence.to_array
  end

let%expect_test "get_all_permutation_vecs" =
  let copy_label = String.of_string
  and values = [| |]
  in
  let num_perms = get_num_perms values in
  let perms = get_all_permutation_vecs ~copy_label values in
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
  let num_perms = get_num_perms values in
  let perms = get_all_permutation_vecs ~copy_label values in
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
  let num_perms = get_num_perms values in
  let perms = get_all_permutation_vecs ~copy_label values in
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
  let num_perms = get_num_perms values in
  let perms = get_all_permutation_vecs ~copy_label values in
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
  let num_perms = get_num_perms values in
  let perms = get_all_permutation_vecs ~copy_label values in
  printf !"%b\n %d\n %{sexp: string list array}\n" (num_perms = Array.length perms) num_perms perms;
  [%expect {|
    true
     6
     ((C B A) (B C A) (C A B) (A C B) (B A C) (A B C))
    |}]

let%expect_test "get_all_permutation_vecs" =
  let copy_label = String.of_string
  and values = [|
      Perm_value.{ label = "A"; num = 1 };
      Perm_value.{ label = "B"; num = 2 };
      Perm_value.{ label = "C"; num = 1 }
    |]
  in
  let num_perms = get_num_perms values in
  let perms = get_all_permutation_vecs ~copy_label values in
  printf !"%b\n %d\n %{sexp: string list array}\n" (num_perms = Array.length perms) num_perms perms;
  [%expect {|
    true
     12
     ((C B B A) (B C B A) (B B C A) (C B A B) (B C A B) (C A B B) (A C B B)
     (B A C B) (A B C B) (B B A C) (B A B C) (A B B C))
    |}]

let%expect_test "get_all_permutation_vecs" =
  let copy_label = String.of_string
  and values = [|
      Perm_value.{ label = "A"; num = 3 };
      Perm_value.{ label = "B"; num = 1 };
      Perm_value.{ label = "C"; num = 1 }
    |]
  in
  let num_perms = get_num_perms values in
  let perms = get_all_permutation_vecs ~copy_label values in
  printf !"%b\n %d\n %{sexp: string list array}\n" (num_perms = Array.length perms) num_perms perms;
  [%expect {|
    true
     20
     ((C B A A A) (B C A A A) (C A B A A) (A C B A A) (B A C A A) (A B C A A)
     (C A A B A) (A C A B A) (A A C B A) (B A A C A) (A B A C A) (A A B C A)
     (C A A A B) (A C A A B) (A A C A B) (A A A C B) (B A A A C) (A B A A C)
     (A A B A C) (A A A B C))
    |}]

let%expect_test "get_all_permutation_vecs" =
  let copy_label = String.of_string
  and values = [|
      Perm_value.{ label = "A"; num = 3 };
      Perm_value.{ label = "B"; num = 5 };
      Perm_value.{ label = "C"; num = 2 }
    |]
  in
  let num_perms = get_num_perms values in
  let perms = get_all_permutation_vecs ~copy_label values in
  printf !"%b %d\n" (num_perms = Array.length perms) num_perms;
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
  let num_perms = get_num_perms values in
  let perms = get_all_permutation_vecs ~copy_label values in
  printf !"%b %d\n" (num_perms = Array.length perms) num_perms;
  [%expect {| true 2520 |}]
