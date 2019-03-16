(* Part 3 *)
type 'a tree = Empty
             | Node of 'a * 'a tree * 'a tree

(* A sample tree containing ints *)
let int_tree : int tree =
  Node (3,
        Node (1,
              Node (4, Empty, Empty), Empty),
        Node (2, Empty, Empty)
       )

(* A sample tree containing strings *)
let str_tree : string tree =
  Node ("love ",
        Node ("really ",
              Node ("I ", Empty, Empty), Empty),
        Node ("OCaml!", Empty, Empty)
       )
(* An additional int tree for assert test *)
let int_tree_extra : int tree =
 Node (10,
       Node (3,
             Node (4, Empty, Empty), Node(7,Empty,Empty)),
       Node (5, Empty, Empty)
      )
(* An additional string tree for assert test *)
let str_tree_extra : string tree =
 Node("Watsons ",
    Node ("love ",
         Node ("really ",
               Node ("I ", Empty, Empty), Empty),
         Node ("drinking ", Empty, Empty)
        ),Node("Soda!",Empty,Empty))
let ints_tree : int list tree =
  Node ([1;3],
        Node ([4;5;6],
              Empty,
              Node ([], Empty, Empty)
             ),
        Node ([],
              Node ([1;6], Empty, Empty),
              Node ([9;2;8],Empty,Empty)
             )
       )
 (* Additional int list tree for assert tests *)
let ints_tree_extra: int list tree =
 Node([0;7],
  Node ([1;3],
        Node ([4;5;6],
              Empty,
              Node ([], Empty, Empty)
             ),
        Node ([],
              Node ([1;6], Empty, Empty),
              Node ([9;2;8],Empty,Empty)
             )
       ),
   Node([],Empty,Empty)
   )
let strs_tree : string list tree =
  Node (["Ocaml!  "; "It "; "must "; "be "],
        Node (["do "; "love "],
              Node (["I "; "really "], Empty, Empty), Empty),
        Node (["your "; "favorite "; "too!"], Empty, Empty)
       )
(* Additional string list tree for assert tests *)
let strs_tree_extra: string list tree =
  Node (["Ocaml!  "; "It "; "must "; "be "],
      Node (["Do "; "love "],
            Node ([], Empty, Empty), Empty),
      Node (["your "; "favorite "; "too!"], Empty, Empty)
     )

let rec reduce (t: 'a tree) (b: 'b) (f: 'a -> 'b -> 'b -> 'b) : 'b =
 match t with
 | Empty -> b
 | Node (v, tl, tr) -> f v (reduce tl b f) (reduce tr b f)
 (* Count number of values in the tree *)
let size (t: 'a tree) : int =
  reduce t 0 (fun v vtl vtr -> 1 + vtl + vtr)
(* adds up all the integers in the tree *)
let sum (t: int tree) : int =
  reduce t 0 (fun v vtl vtr -> v + vtl + vtr)
(* Calculate the product of all the values *)
let product (t: int tree) : int =
  reduce t 1 (fun v vtl vtr -> v * vtl * vtr)
(* Count number of chars in a string tree *)
let charcount (t: string tree) : int =
  reduce t 0 (fun v vtl vtr -> String.length v + vtl + vtr)
(* Concat all the string in a string tree *)
let concat (t: string tree) : string =
  reduce t "" (fun v vtl vtr -> vtl ^ v ^ vtr)
(* Count the total number of elements in a 'a list tree *)
let list_tree_size (t: 'a list tree) : int =
  reduce t 0 (fun lst vtl vtr -> List.length lst + vtl + vtr)
(* Sum up all the integers in a int list tree *)
let list_tree_sum (t: int list tree) : int =
  reduce t 0 (fun lst vtl vtr -> List.fold_left (fun x y -> x + y ) 0 lst + vtl + vtr)
(* Calculate the product of all the integers in a int list tree *)
let list_tree_product (t: int list tree) : int =
  reduce t 1 (fun lst vtl vtr -> List.fold_left (fun x y -> x * y ) 1 lst * vtl * vtr)
(* Count all the chars in a string list tree *)
let list_tree_charcount (t: string list tree) : int =
  reduce t 0 (fun lst vtl vtr -> List.fold_left (fun x y -> x + String.length y ) 0 lst + vtl + vtr)
(* Concat all the string in a string list tree *)
let list_tree_concat (t: string list tree) : string =
  reduce t "" (fun lst vtl vtr -> vtl ^ List.fold_left (fun x y -> x ^ y ) "" lst ^ vtr)

let () =
  print_string "Testing part 3 ... " ;
  try
    assert (size str_tree = 4);
    assert (size int_tree = 4);
    assert (size int_tree_extra = 5);
    assert (sum int_tree = 10);
    assert (sum int_tree_extra = 29);
    assert (product int_tree = 24);
    assert (product int_tree_extra = 4200);
    assert (charcount str_tree = 20);
    assert (charcount str_tree_extra = 36);
    assert (concat str_tree = "I really love OCaml!");
    assert (concat str_tree_extra = "I really love drinking Watsons Soda!");
    assert (list_tree_size strs_tree = 11);
    assert (list_tree_size ints_tree = 10);
    assert (list_tree_size ints_tree_extra = 12);
    assert (list_tree_sum ints_tree = 45);
    assert (list_tree_sum ints_tree_extra = 52);
    assert (list_tree_product ints_tree = 311040);
    assert (list_tree_product ints_tree_extra = 0);
    assert (list_tree_charcount strs_tree = 54);
    assert (list_tree_charcount strs_tree_extra = 45);
    assert (list_tree_concat strs_tree =
              "I really do love Ocaml!  It must be your favorite too!");
    assert (list_tree_concat strs_tree_extra =
              "Do love Ocaml!  It must be your favorite too!");
    print_string "tests passed.\n"
  with
    Assert_failure (file, line, column) ->
    let msg = "\n\n\nAssert failed on line " ^ string_of_int line ^
                ", column " ^ string_of_int column ^ "\n\n\n\n"
    in print_string msg
