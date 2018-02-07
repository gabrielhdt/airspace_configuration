let argmax cmp xs =
  let rec loop rxs marg = match rxs with
    | [] -> marg
    | hd :: tl ->
      loop tl (if cmp hd marg >= 0 then hd else marg)
  in
  loop xs
