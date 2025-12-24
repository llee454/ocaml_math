(**
  This module defines functions that can be used to group elements
  together.

  Given a set of elements E the transitive closure is a set of subsets
  called groups G that satisfy the following property:

  for every pair of elements e0 and en, if there exists a sequence
  of elements e0, e1, e2, ..., en such that every element ei in
  this sequence shares one or more key values with the following
  element, then e0 and en are in the same group.

  As a corrolary of this definition, if e0 is in a group g, g also
  contains every element that shares a key with e0.

  Why would we use this? Imagine that you have a set of people and
  every person has a list of addresses and we want to cluster together
  people who lived at the same address. We can use this module to
  group them together.
*)
open Core

(** represents the type of elements that will be grouped   
  together *)
module type Element = sig
  type t

  (**
    Represents the keys that elements have. These will be used to
    determine if two elements "overlap" and should be grouped together.
  *)

  module K : sig
    type t [@@deriving compare, equal, hash, sexp]
  end

  module Set : Set.S with type Elt.t = K.t

  (** Accepts an elements and returns the keys that it is associated with. *)
  val get_keys : t -> Set.t
end

module Make (E : Element) = struct
  module KTable = Hashtbl.Make (E.K)

  (** Accepts a list of elements and returns their transitive closure. *)
  type t = E.t

  let get xs =
    let get_next_group_id =
      let next_group_id = ref 0 in
      fun () ->
        let x = !next_group_id in
        incr next_group_id;
        x
    in
    let key_group_tbl : int KTable.t = KTable.create () in
    let group_keys_tbl : E.Set.t Int.Table.t = Int.Table.create () in
    let group_records_tbl : E.t list Int.Table.t = Int.Table.create () in

    let make_empty_group () =
      let group_id = get_next_group_id () in
      Hashtbl.add_exn group_keys_tbl ~key:group_id ~data:E.Set.empty;
      Hashtbl.add_exn group_records_tbl ~key:group_id ~data:[];
      group_id
    in
    let add_record (x : t) keys group_id =
      Set.iter keys ~f:(fun key -> Hashtbl.set key_group_tbl ~key ~data:group_id);
      Hashtbl.update group_keys_tbl group_id ~f:(function
        | None -> keys
        | Some existing -> Set.union existing keys );
      Hashtbl.add_multi group_records_tbl ~key:group_id ~data:x
    in
    let combine_groups group_id ~into_group_id =
      let records = Hashtbl.find_and_remove group_records_tbl group_id |> Option.value_exn in
      let keys = Hashtbl.find_and_remove group_keys_tbl group_id |> Option.value_exn in
      Set.iter keys ~f:(fun key ->
          Hashtbl.update key_group_tbl key ~f:(function
            | None -> failwiths ~here:[%here] "Error: an internal error occured." () [%sexp_of: unit]
            | Some _ -> into_group_id ) );
      Hashtbl.update group_keys_tbl into_group_id ~f:(function
        | None -> failwiths ~here:[%here] "Error: an internal error occured." () [%sexp_of: unit]
        | Some existing -> Set.union existing keys );
      Hashtbl.update group_records_tbl into_group_id ~f:(function
        | None -> failwiths ~here:[%here] "Error: an internal error occured." () [%sexp_of: unit]
        | Some ll -> List.concat_no_order [ ll; records ] )
    in
    List.iter xs ~f:(fun (x : E.t) ->
        let keys = E.get_keys x in
        let group_ids =
          Set.fold keys ~init:Int.Set.empty ~f:(fun group_ids k ->
              Hashtbl.find_and_call ~if_found:(Set.add group_ids)
                ~if_not_found:(fun (_ : KTable.key) -> group_ids)
                key_group_tbl k )
          |> Set.to_sequence
        in
        let into_group_id = Sequence.hd group_ids |> Option.value_or_thunk ~default:make_empty_group in
        add_record x keys into_group_id;
        Sequence.drop group_ids 1 |> Sequence.iter ~f:(combine_groups ~into_group_id) );

    group_records_tbl
end

module Example1 = Make (struct
  type t = String.Set.t * int

  module K = String
  module Set = String.Set

  let get_keys = fst
end)

let%expect_test "Transitive Closure 1" =
  [ String.Set.of_list [ "A" ], 1; String.Set.of_list [ "B" ], 2 ]
  |> Example1.get
  |> Hashtbl.data
  |> printf !"%{sexp: ((String.Set.t * int) list) list}\n";
  [ String.Set.of_list [ "A" ], 1; String.Set.of_list [ "B" ], 2; String.Set.of_list [ "A"; "B" ], 3 ]
  |> Example1.get
  |> Hashtbl.data
  |> printf !"%{sexp: ((String.Set.t * int) list) list}\n";
  [
    String.Set.of_list [ "A" ], 1;
    String.Set.of_list [ "B" ], 2;
    String.Set.of_list [ "A"; "B" ], 3;
    String.Set.of_list [ "C" ], 4;
  ]
  |> Example1.get
  |> Hashtbl.data
  |> printf !"%{sexp: ((String.Set.t * int) list) list}\n";
  [%expect
    {|
    ((((B) 2)) (((A) 1)))
    ((((B) 2) ((A) 1) ((A B) 3)))
    ((((C) 4)) (((B) 2) ((A) 1) ((A B) 3))) |}]
