open! Core

let rec get_seqs ~len ~sum () =
  match () with
  | () when len = 0 -> []
  | () when len = 1 -> [ [sum] ]
  | _ -> 
    List.init (sum + 1) ~f:(fun i ->
      List.map
        (get_seqs ~len:(len - 1) ~sum:(sum - i) ()) 
        ~f:(List.cons i)
    ) |> List.concat

let%expect_test "get_seqs" =
  get_seqs ~len:5 ~sum:5 ()
  |> printf !"%{sexp: int list list}\n";
  [%expect {|
    ((0 0 0 0 5) (0 0 0 1 4) (0 0 0 2 3) (0 0 0 3 2) (0 0 0 4 1) (0 0 0 5 0)
     (0 0 1 0 4) (0 0 1 1 3) (0 0 1 2 2) (0 0 1 3 1) (0 0 1 4 0) (0 0 2 0 3)
     (0 0 2 1 2) (0 0 2 2 1) (0 0 2 3 0) (0 0 3 0 2) (0 0 3 1 1) (0 0 3 2 0)
     (0 0 4 0 1) (0 0 4 1 0) (0 0 5 0 0) (0 1 0 0 4) (0 1 0 1 3) (0 1 0 2 2)
     (0 1 0 3 1) (0 1 0 4 0) (0 1 1 0 3) (0 1 1 1 2) (0 1 1 2 1) (0 1 1 3 0)
     (0 1 2 0 2) (0 1 2 1 1) (0 1 2 2 0) (0 1 3 0 1) (0 1 3 1 0) (0 1 4 0 0)
     (0 2 0 0 3) (0 2 0 1 2) (0 2 0 2 1) (0 2 0 3 0) (0 2 1 0 2) (0 2 1 1 1)
     (0 2 1 2 0) (0 2 2 0 1) (0 2 2 1 0) (0 2 3 0 0) (0 3 0 0 2) (0 3 0 1 1)
     (0 3 0 2 0) (0 3 1 0 1) (0 3 1 1 0) (0 3 2 0 0) (0 4 0 0 1) (0 4 0 1 0)
     (0 4 1 0 0) (0 5 0 0 0) (1 0 0 0 4) (1 0 0 1 3) (1 0 0 2 2) (1 0 0 3 1)
     (1 0 0 4 0) (1 0 1 0 3) (1 0 1 1 2) (1 0 1 2 1) (1 0 1 3 0) (1 0 2 0 2)
     (1 0 2 1 1) (1 0 2 2 0) (1 0 3 0 1) (1 0 3 1 0) (1 0 4 0 0) (1 1 0 0 3)
     (1 1 0 1 2) (1 1 0 2 1) (1 1 0 3 0) (1 1 1 0 2) (1 1 1 1 1) (1 1 1 2 0)
     (1 1 2 0 1) (1 1 2 1 0) (1 1 3 0 0) (1 2 0 0 2) (1 2 0 1 1) (1 2 0 2 0)
     (1 2 1 0 1) (1 2 1 1 0) (1 2 2 0 0) (1 3 0 0 1) (1 3 0 1 0) (1 3 1 0 0)
     (1 4 0 0 0) (2 0 0 0 3) (2 0 0 1 2) (2 0 0 2 1) (2 0 0 3 0) (2 0 1 0 2)
     (2 0 1 1 1) (2 0 1 2 0) (2 0 2 0 1) (2 0 2 1 0) (2 0 3 0 0) (2 1 0 0 2)
     (2 1 0 1 1) (2 1 0 2 0) (2 1 1 0 1) (2 1 1 1 0) (2 1 2 0 0) (2 2 0 0 1)
     (2 2 0 1 0) (2 2 1 0 0) (2 3 0 0 0) (3 0 0 0 2) (3 0 0 1 1) (3 0 0 2 0)
     (3 0 1 0 1) (3 0 1 1 0) (3 0 2 0 0) (3 1 0 0 1) (3 1 0 1 0) (3 1 1 0 0)
     (3 2 0 0 0) (4 0 0 0 1) (4 0 0 1 0) (4 0 1 0 0) (4 1 0 0 0) (5 0 0 0 0))
    |}]

let get_num_seqs ~len ~sum () = Basic.binom_coeff ~n:(sum + len - 1) ~k:sum () [@@inline]

let%expect_test "get_num_seqs" =
  let len = 5 and sum = 5 in
  get_num_seqs ~len ~sum ()
  |> Bigint.to_string
  |> printf "%d %s" (List.length (get_seqs ~len ~sum ()));
  [%expect {| 126 126 |}]

(* let rec get_seq ~(sum : int) ~(len : int) (nth : Bigint.t) =
  match () with
  | () when len = 0 -> Some []
  | () when len = 1 && Bigint.(nth > Bigint.zero) -> None
  | () when len = 1 && Bigint.(nth = Bigint.zero) -> Some [sum]
  | _ -> begin
    let first_val = ref 0 in
    let rem_opt = ref None in
    begin try
      let offset = ref (Bigint.of_int 0) in
      for i = 0 to (sum + 1) do
        let num_seqs = get_num_seqs ~len:(len - 1) ~sum:(sum - i) ()  in
        if Bigint.(nth < !offset + num_seqs)
        then begin
          first_val := i;
          rem_opt := get_seq ~len:(len - 1) ~sum:(sum - i) Bigint.(nth - !offset);
          raise Exit
        end; 
        offset := Bigint.(!offset + num_seqs)
      done;
    with
    | Exit -> ()
    end;
    Option.map !rem_opt ~f:(fun rem -> List.cons !first_val rem);
  end *)

let get_seq ~(sum : int) ~(len : int) (nth : Bigint.t) =
  let res = Queue.create ~capacity:len ()
  and rem_nth = ref nth
  and rem_len = ref len
  and rem_sum = ref sum
  in
  for _i = 0 to len - 1 do
    begin try
        let offset = ref Bigint.zero
        and next_len = !rem_len - 1
        in
        for x0 = 0 to !rem_sum do
          let next_sum = !rem_sum - x0 in
          let num_seqs = get_num_seqs ~len:next_len ~sum:next_sum () in
          if Bigint.(!rem_nth < !offset + num_seqs)
          then begin
            Queue.enqueue res x0;
            rem_nth := Bigint.(!rem_nth - !offset);
            rem_len := next_len;
            rem_sum := next_sum;
            raise Exit
          end;
          offset := Bigint.(!offset + num_seqs)
        done;
      with | Exit -> ()
    end;
  done;
  res

let%expect_test "get_seq" =
  get_seq ~sum:1 ~len:1 (Bigint.zero)
  |> printf !"%{sexp: int Queue.t}";
  [%expect {| (1) |}]

let%expect_test "get_seq" =
  get_seq ~sum:5 ~len:5 (Bigint.of_int 48)
  |> printf !"%{sexp: int Queue.t}";
  [%expect {| (0 3 0 2 0) |}]

let%expect_test "get_seq" =
  get_seq ~sum:5 ~len:5 (Bigint.of_int 125)
  |> printf !"%{sexp: int Queue.t}";
  [%expect {| (5 0 0 0 0) |}]

(**
  Accepts two arguments: [len] and [sum]; and returns a random integer
  sequence, drawn from a uniform distribution, having length [len] and sum
  [sum].
*)
(* let rand_seq_sum_uniform ~len ~sum () =
  get_num_seqs ~len ~sum ()
  |> Bigint.random 
  |> get_seq ~sum ~len *)

let get_proportion_of_seqs_in_next_sum ~sum ~next_sum ~len  () =
  let next_len = len - 1
  and res = ref 1.0
  in
  for i = 1 to next_sum do
    res := !res *. ((next_sum + next_len - i)//(sum + len - i))
  done;
  for i = next_sum + 1 to sum do
    res := !res *. (i//(sum + len - i))
  done;
  !res

let%expect_test "get_prop" =
  let res = get_proportion_of_seqs_in_next_sum ~sum:5 ~next_sum:3 ~len:5 ()
  and ref = Bignum.(
    Bignum.of_bigint (get_num_seqs ~len:4 ~sum:3 ()) /
    Bignum.of_bigint (get_num_seqs ~len:5 ~sum:5 ()))
  in
  printf !"%f %{sexp: Bignum.t}" res ref;
  [%expect {| 0.158730 (0.158730158 + 23/31500000000) |}]

let rand_seq_sum_uniform ~len ~sum () =
  let res = Array.create ~len 0
  and rem_len = ref len
  and rem_sum = ref sum
  in
  for i = 0 to len - 1 do
    begin try
        let dir = Random.float 1.0
        and p_acc = ref 0.0
        in
        for x0 = 0 to !rem_sum do
          let next_sum = !rem_sum - x0 in
          let p = get_proportion_of_seqs_in_next_sum ~sum:!rem_sum ~next_sum ~len:!rem_len () in 
          if Float.(dir < !p_acc + p)
          then begin
            res.(i) <- x0;
            rem_len := !rem_len - 1;
            rem_sum := next_sum;
            raise Exit
          end;
          p_acc := !p_acc +. p;
        done;
      with | Exit -> ()
    end;
  done;
  res


let%expect_test "rand_seq_sum_uniform_aux" =
  rand_seq_sum_uniform ~len:10 ~sum:100 ()
  |> printf !"%{sexp: int array}\n";
  [%expect {| (11 16 14 6 8 1 14 0 7 23) |}]

let%expect_test "rand_seq_sum_uniform" =
  rand_seq_sum_uniform ~len:0 ~sum:0 ()
  |> printf !"%{sexp: int array}";
  [%expect {| () |}]

let%expect_test "rand_seq_sum_uniform" =
  rand_seq_sum_uniform ~len:0 ~sum:0 ()
  |> printf !"%{sexp: int array}";
  [%expect {| () |}]

let%expect_test "rand_seq_sum_uniform" =
  rand_seq_sum_uniform ~len:5 ~sum:0 ()
  |> printf !"%{sexp: int array}";
  [%expect {| (0 0 0 0 0) |}]

let%expect_test "rand_seq_sum_uniform" =
  rand_seq_sum_uniform ~len:5 ~sum:1 ()
  |> printf !"%{sexp: int array}";
  [%expect {| (0 1 0 0 0) |}]

let%expect_test "rand_seq_sum_uniform" =
  rand_seq_sum_uniform ~len:1 ~sum:5 ()
  |> printf !"%{sexp: int array}";
  [%expect {| (5) |}]

let%expect_test "rand_seq_sum_uniform" =
  rand_seq_sum_uniform ~len:5 ~sum:10 ()
  |> printf !"%{sexp: int array}";
  [%expect {| (2 3 3 1 1) |}]

let random_composition n sum =
  if n <= 0 || sum < 0 then
    invalid_arg "random_composition";

  let m = sum + n - 1 in
  let cuts = Hash_set.create (module Int) in

  while Hash_set.length cuts < n - 1 do
    Hash_set.add cuts (Random.int m)
  done;

  let cuts =
    Hash_set.to_list cuts
    |> List.sort ~compare:Int.compare
  in

  let rec build prev cuts acc =
    match cuts with
    | [] ->
        List.rev ((m - prev - 1) :: acc)
    | c :: rest ->
        build c rest ((c - prev - 1) :: acc)
  in

  build (-1) cuts []

let%expect_test "random_composition" =
  random_composition 2 5
  |> printf !"%{sexp: int list}\n";
  [%expect {| (3 2) |}]