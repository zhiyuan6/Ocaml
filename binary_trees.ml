(* Part 4 *)
type 'a btree = Nil
              | Leaf of 'a
              | Fork of 'a btree * 'a * 'a btree

(* Insert elements into tree and keep the tree invariants  *)
let rec insert_by (f: ('a -> 'a -> int)) (v: 'a) (t: 'a btree) : 'a btree =
  match t with
  | Nil -> Leaf v
  | Leaf r ->  if compare v r < 0 then Fork (Leaf v, r, Nil)
                   else if compare v r > 0 then Fork (Nil, r, Leaf v)
                   else Leaf r
  | Fork (tl, r, tr) -> if compare v r < 0 then Fork (insert_by compare v tl, r, tr)
                   else if compare v r > 0 then Fork (tl, r, insert_by compare v tr)
                   else Fork (tl, r, tr)
(* Insert elements in a list to the tree *)
let from_list (f: 'a->'a->int) (lst:'a list) : 'a btree =
  let accum = Nil
  in
  let f x y =
    insert_by compare y x
  in
  List.fold_left f accum lst
(* Reduce function for this type of tree *)
let rec reduce (t: 'a btree) (b: 'b) (f: 'b -> 'a -> 'b -> 'b) : 'b =
  match t with
  | Nil -> b
  | Leaf v -> f (reduce Nil b f) v (reduce Nil b f)
  | Fork (tl, v, tr) -> f (reduce tl b f) v (reduce tr b f)
(* Take all the elements in the tree and put into a sorted list *)
let to_list (t: 'a btree): 'a list  =
  reduce t [] (fun vtl v vtr -> vtl@[v]@vtr)

(* Lab_07 *)
(* 1. I use "List.fold_left".
2. Because we may want to drop duplicates from the left.
3. If we drop them from the right, the order may be different. *)
let rem_dups (lst: 'a list) : 'a list =
  let accum = [] in
  let f x y =
    if List.mem y x then x
    else y::x
  in
  List.rev (List.fold_left f accum lst)

(* Check the second and third invariants *)
let rec form_check (t: 'a btree) : bool =
(* Check all the elements in the left subtree are smaller than parent and those in the right are greater that parent *)
  let cmp_tree (t: 'a btree): bool=
    match t with
    | Fork(Leaf vl, v, Leaf vr)  -> if compare vl v > 0 || compare vr v < 0 then false else true
    | Fork(Leaf vl, v, Nil) -> if compare vl v > 0 then false else true
    | Fork(Nil, v, Leaf vr) -> if compare vr v < 0 then false else true
    | Fork(Fork(_, vl, _), v, Leaf vr) -> if compare vl v > 0 || compare vr v < 0 then false else true
    | Fork(Fork(_, vl, _), v, Nil) -> if compare vl v > 0 then false else true
    | Fork(Leaf vl, v, Fork(_, vr, _)) -> if compare vl v > 0 || compare vr v < 0 then false else true
    | Fork(Nil, v, Fork(_, vr, _)) -> if compare vr v < 0 then false else true
    | _ -> true
  in
    match t with
(* Check invalid pattern *)
    | Fork(Nil, v, Nil)-> false
    | Fork(tl, v, tr) -> form_check tl && cmp_tree (Fork(tl, v, tr)) && form_check tr
    | _ -> true

(* Check all three invariants *)
let check (t: 'a btree) : bool =
  if rem_dups (to_list t) <> to_list t then false
  else
  form_check t


let () =
 print_string "Testing part 4 ... " ;
 try
   assert (insert_by compare 4 Nil = Leaf 4);
   assert (insert_by compare 2 (insert_by compare 4 Nil) =
             Fork (Leaf 2, 4, Nil));
   assert (insert_by compare 4 (insert_by compare 2 Nil) =
             Fork (Nil, 2, Leaf 4));
   assert (insert_by compare 4 (insert_by compare 4 Nil) =
             insert_by compare 4 Nil);
   assert (List.sort compare [4;2;5;3;6;7;8] =
              to_list (from_list compare [4;2;5;3;6;7;8]));
   assert (rem_dups [1;3;3;5;1;7;2;5;7;2] = [1; 3; 5; 7; 2]);
   assert (check Nil);
   assert (check (Leaf 'c'));
   assert (check (from_list compare [4;2;5;3;6;7;8]));
   assert (not (check (Fork (Nil, 4, Nil))));
   assert (not (check (Fork (Leaf 3, 3, Nil))));
   (* Add more asserts here as you need them *)
   assert (insert_by compare 1 (insert_by compare 4 (insert_by compare 2 Nil)) =
              Fork (Leaf 1, 2, Leaf 4));
   assert (insert_by compare 6 (insert_by compare 1 (insert_by compare 4 (insert_by compare 2 Nil))) =
               Fork (Leaf 1, 2, Fork (Nil, 4, Leaf 6)));
   assert (insert_by compare 0 (insert_by compare 6 (insert_by compare 1 (insert_by compare 4 (insert_by compare 2 Nil)))) =
               Fork (Fork (Leaf 0, 1, Nil), 2, Fork (Nil, 4, Leaf 6)));
   assert (from_list compare [4;2;5;3;6;7;8] =
              Fork (Fork (Nil, 2, Leaf 3), 4,
                    Fork (Nil, 5, Fork (Nil, 6, Fork (Nil, 7, Leaf 8)))));
   assert (from_list compare [] = Nil);
   assert (from_list compare [4] = Leaf 4);
   assert (from_list compare [4;5] = Fork (Nil, 4, Leaf 5));
   assert (List.sort compare [] =
              to_list (from_list compare []));
   assert (List.sort compare [4] =
              to_list (from_list compare [4]));
   assert (List.sort compare [1;2;3;4;5;6;7;8;9;10;11;12] =
              to_list (from_list compare [1;2;3;4;5;6;7;8;9;10;11;12]));
   assert (List.sort compare [10;9;8;7;6;5;4;3;2;1] =
              to_list (from_list compare [10;9;8;7;6;5;4;3;2;1]));
   assert (rem_dups [] = []);
   assert (rem_dups [2;3;4] = [2;3;4]);
   assert (check (Fork (Leaf 'c','d',Nil)));
   assert (check (from_list compare [1;2;3;4;5;6;7;8;9;10;11;12]));
   assert (not (check (Fork ((Fork (Nil, 4, Nil)), 10, Fork(Leaf 12, 20, Nil)))));
   assert (not (check (Fork (Leaf 3, 3, Nil))));
   print_string "tests passed.\n"
 with
   Assert_failure (file, line, column) ->
   let msg = "\n\n\nAssert failed on line " ^ string_of_int line ^
               ", column " ^ string_of_int column ^ "\n\n\n\n"
   in print_string msg
