let argmax cmp xs =
  List.fold_left cmp (List.hd xs) xs

(* Temporary considering functor approach *)
(* let dummystate = Airconf.dummy *)

let random_elt lst =
  List.nth lst (Random.int (List.length lst))
