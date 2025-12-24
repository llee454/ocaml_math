(**
  The most common reason for using this module is to create random vectors
  whose probabilities are uniformly distributed across some domain and to
  create random permutations while ensuring that every permutation has an
  equal probability of being generated.

  A fillable vector is a vector that will "fill" based on some constraint. For
  example, a vector whose elements must equal some sum or a vector whose
  values must be drawn from some set.

  We create a binary tree whose leaves correspond to the vector slots and
  then use the tree to both randomly select values to "fill in" and to keep
  track of which values have yet to be "filled."
*)
open Core

(**
  Fillable vector.

  We have a vector where we will repeatedly select a random slot and,
  if the slot is not "filled," we will add an additional new element to
  the slot. We want to do this efficiently, i.e. we want avoid randomly
  selecting slots that are already "filled" and we want to ensure that
  the process gives as uniform a probability distribution over all slots
  as possible.

  We create a binary tree over all of the slots. The leaves represent
  slots and report two values, the number of elements inserted and the
  number of elements that can be inserted before the slot is "filled." The
  nodes report the number of slots leaves/slots below them that are
  available/not filled.

  Each iteration, we look at the root node and choose a number less than or
  equal to the number of available slots. We then walk through the tree to
  find the nth slot, insert our element, and recursively update the nodes.
*)
module Tree = struct
  type 'a t =
  | Node of {
    num: int; (** the number of leaves in this tree *)
    mutable num_available: int; (** the number of unfilled leaves in this tree *)
    left: 'a t;
    right_opt: 'a t option }
  | Leaf of { mutable value: 'a; mutable full: bool }
  [@@deriving sexp]
end  

(**
  Accepts a fillable tree and returns a sequence of the leaf values in
  left-most order.
*)
let rec get_leaves : 'a Tree.t -> 'a Sequence.t = function
| Tree.Leaf info -> Sequence.singleton info.value
| Tree.Node info ->
  Sequence.append
    (get_leaves info.left)
    (Option.value_map info.right_opt ~default:Sequence.empty ~f:(fun right ->
      get_leaves right
    ))

(** Returns the number of leaves in the given tree. *)
let get_num = function
| Tree.Node info -> info.num
| Tree.Leaf _ -> 1

 (**
   Accepts a tree and returns the number of leafs that are not full
   within it.
 *)
 let get_num_available = function
 | Tree.Node info -> info.num_available
 | Tree.Leaf info -> if info.full then 0 else 1

(**
  Accepts two arguments: [f], a function that accepts a slot index and
  returns the slot's data; and [num]; and creates a fillable vector tree
  that has [num] slots where every slot is filled using [f].
*)
let rec create ~f num =
  if num = 0
  then None
  else Some (create_aux ~f 0 num)

and create_aux ~f start num =
  let depth = float num |> Float.log2 |> Float.iround_up_exn in
  if depth = 0
  then Tree.Leaf { value = f start; full = false }
  else begin
    let num_right = num / 2 in
    let num_left  = num - num_right in
    let left = create_aux ~f start num_left
    and right_opt =
      if num_right = 0
      then None
      else Some (create_aux ~f (start + num_left) num_right)
    in
    Tree.Node { num; num_available = num; left; right_opt }
  end

let%expect_test "create" =
  create 5 ~f:(fun i -> i) |> printf !"%{sexp: int Tree.t option}"; 
  [%expect {|
    ((Node (num 5) (num_available 5)
      (left
       (Node (num 3) (num_available 3)
        (left
         (Node (num 2) (num_available 2) (left (Leaf (value 0) (full false)))
          (right_opt ((Leaf (value 1) (full false))))))
        (right_opt ((Leaf (value 2) (full false))))))
      (right_opt
       ((Node (num 2) (num_available 2) (left (Leaf (value 3) (full false)))
         (right_opt ((Leaf (value 4) (full false)))))))))
    |}]

module Tree_update_op = struct
  (**
    Indicates whether or not a tree update filled a slot, unfilled a
    slot, or had no impact on the number of filled slots.
  *)
  type t = Filled | Unfilled | Noop
  [@@deriving sexp]
end

(** 
  Accepts four arguments: [f], a function that accepts a slot value, and
  returns a new slot value; [is_full], another function that accepts a
  slot value and returns true iff the slot is "full"; [i], a slot index;
  and [tree], a fillable vector tree; and updates the i-th slot's value
  using [f].

  If, after the update, the slot is full according to [is_full], this
  function marks the slot as full and updates the ancestor nodes
  accordingly.

  If the [unfilled_only] flag is set to true, the index [i] refers to the
  "i-th unfilled slot." Exercise care, when using this flag as the i-th
  unfilled slot may change as slots are filled and unfilled.

  Note: [is_full] only gets called on a slot after we insert a value into
  that slot.
*)
let rec update ?(unfilled_only = false) ~f ~is_full i tree =
  let _ = update_aux ~unfilled_only ~f ~is_full i tree in ()

and emsg = "Error: an error occured while trying to update a fillable vector tree. Invalid index."
and update_aux ?(unfilled_only = false) ~f ~is_full i = function
  | Tree.Leaf info ->
    if i = 0
    then begin
      info.value <- f info.value;
      let full = is_full info.value in
      let res = match info.full, full with
      | true, false -> Tree_update_op.Unfilled
      | false, true -> Tree_update_op.Filled
      | _ -> Tree_update_op.Noop
      in
      info.full  <- full;
      res
    end else failwiths ~here:[%here] emsg i [%sexp_of: int]
  | Tree.Node info ->
    let num_left = if unfilled_only
      then get_num_available info.left
      else get_num info.left
    in
    let filled_slot = if i < num_left
      then update_aux ~unfilled_only ~f ~is_full i info.left
      else begin
        match info.right_opt with 
        | None -> failwiths ~here:[%here] emsg i [%sexp_of: int]
        | Some right -> update_aux ~unfilled_only ~f ~is_full (i - num_left) right
      end
    in
    let () = match filled_slot with
      | Tree_update_op.Filled -> info.num_available <- info.num_available - 1
      | Tree_update_op.Unfilled -> info.num_available <- info.num_available + 1
      | _ -> ()
    in
    filled_slot

let%expect_test "create" =
  let tree = create 5 ~f:(fun i -> i) |> Option.value_exn in
  update 0 tree ~f:(fun x -> 2*x) ~is_full:(Fn.const true);
  update 4 tree ~f:(fun x -> 2*x) ~is_full:(Fn.const true);
  update 1 tree ~unfilled_only:true ~f:(fun x -> 2*x) ~is_full:(Fn.const true);
  printf !"%{sexp: int Tree.t}" tree;
  [%expect {|
    (Node (num 5) (num_available 2)
     (left
      (Node (num 3) (num_available 1)
       (left
        (Node (num 2) (num_available 1) (left (Leaf (value 0) (full true)))
         (right_opt ((Leaf (value 1) (full false))))))
       (right_opt ((Leaf (value 4) (full true))))))
     (right_opt
      ((Node (num 2) (num_available 1) (left (Leaf (value 3) (full false)))
        (right_opt ((Leaf (value 8) (full true))))))))
    |}]
