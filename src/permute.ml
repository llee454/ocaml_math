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
    let tree = Fillable_vector.create ~f:(Fn.const None) len |> Option.value_exn in
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
      let tree = Fillable_vector.create ~f:(Fn.const 0) len |> Option.value_exn in
      for i = 0 to num_ones - 1 do
        let j = Random.int (len - i) in
        Fillable_vector.update j tree ~unfilled_only:true ~f:(Fn.const 1) ~is_full:(Fn.const true)
      done;
      Fillable_vector.get_leaves tree

let%expect_test "gen_rand_binary_seq" =
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
  [%expect {||}]
