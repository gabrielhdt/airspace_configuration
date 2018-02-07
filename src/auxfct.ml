let argmax cmp xs =
  List.fold_left cmp (List.hd xs) xs
