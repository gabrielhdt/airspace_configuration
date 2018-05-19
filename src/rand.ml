let roulette_result v wheel =
  let n = Array.length wheel in
  let rec loop i =
    if i >= n then failwith "roulette_result"
    else
      let ki, vi = wheel.(i) in
      if v < vi then ki else loop (i + 1)
  in
  loop 0


let roulette wheel =
  if wheel = [||] then failwith "roulette"
  else
    let v = Random.float 1.0 in
    (*    Printf.printf "roulette %f\n" v;*)
    let j = roulette_result v wheel in
    j


let debug_print a =
  Array.iter (fun (i, x) -> Printf.printf "(%d,%.4f) " i x) a ;
  Printf.printf "\n" ;
  flush stdout


let debug_save values =
  let ch = open_out "values" in
  Printf.fprintf ch "values: %d\n" (Array.length values) ;
  Array.iteri
    (fun j (i, v) -> Printf.fprintf ch "%d: (%d, %.10e)\n" j i v)
    values ;
  close_out ch


let make_wheel values =
  let epsilon = 1e-11 in
  let q = Array.length values in
  let total, values =
    let t = Array.fold_left (fun acc (_, v) -> v +. acc) 0. values in
    if t = 0. then (float q, Array.map (fun (i, _) -> (i, 1.)) values)
      (* distribution uniforme *)
    else (t, values)
  in
  let proba = Array.map (fun (i, v) -> (i, v /. total)) values in
  (*  Printf.printf "Total= %.4f\n" total;
    debug_print proba;*)
  let cmp a b = compare (snd a) (snd b) in
  Array.sort cmp proba ;
  let rec make_wheel acc i =
    if i >= q then []
    else
      let j, p = proba.(i) in
      let v = p +. acc in
      (j, v) :: make_wheel v (i + 1)
  in
  let wheel = Array.of_list (make_wheel 0. 0) in
  let k, v = wheel.(q - 1) in
  if abs_float (v -. 1.) <= epsilon then wheel.(q - 1) <- (k, 1.)
  else (
    debug_save values ;
    failwith (Printf.sprintf "make_wheel: v=%.10e" v) ) ;
  (*  debug_print wheel;*)
  wheel

(*
let a= [|2,0.10;3,0.04;4,0.06|];;
let a= [|2,-.0.10;3,-.0.04;4,-.0.06|];;
let w= make_wheel a;;
*)
