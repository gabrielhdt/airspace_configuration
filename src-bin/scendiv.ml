open Yojson.Basic.Util
open Printf

let nchunks = int_of_string Sys.argv.(2)
let basefname = Sys.argv.(1)

(** [drop n x] drops the [n] first elements of x. Returns empty list if [n]
    is greater than the length of [x] *)
let drop n xs =
  let rec loop ys k =
    if k >= n then ys else
      loop (List.tl ys) (succ k) in
  if n >= List.length xs then [] else loop xs 0

(** [take n x] takes the first [n] elements of [x]. Returns the full list if
    [n] is greater than the length of [x] *)
let take n xs =
  let rec loop k ys =
    if k >= n then [] else (List.hd ys) :: loop (succ k) (List.tl ys) in
  if n >= List.length xs then xs else List.rev @@ loop 0 xs

let slice b e xs = take (e - b) @@ drop b xs

let write_partial_scens data (ndiv : int)
    (cardperdiv : int) (nlastdiv : int) =
  let rec loop k = (* Iteration over the file number *)
    if k >= ndiv then () else
      let fname = basefname ^ (string_of_int k) in
      let sliced = List.map (fun (em, traf) ->
          let ind_begin, ind_end = k * cardperdiv, (k + 1) * cardperdiv in
          (em, slice ind_begin ind_end traf)) data in
      (* Build the string to be written to file *)
      let traffjsonstr = List.fold_left (fun outstr (em, traf) ->
          let opening = sprintf "\t\"%s\": [" em in
          let traff = List.fold_left (fun trafstr trafelt ->
              trafstr ^ sprintf "%d," trafelt) "" traf in
          let trafofem = opening ^ traff ^ "],\n" in
          outstr ^ trafofem
        ) "{\n" sliced
      (* Now write the string to file *)
      and outfile = open_out fname in
      begin
        fprintf outfile "%s\n}" traffjsonstr;
        close_out outfile;
        (* And loop *)
        loop (k + 1)
      end
  in
  loop 0

let () =
  let scenjson = Yojson.Basic.from_file Sys.argv.(1) in
  let mod2json = to_assoc scenjson in
  let mod2traff = List.map (fun (sec, json) ->
      (sec, List.map to_int @@ to_list json)) mod2json in
  let traflength = List.length @@ snd @@ List.hd mod2traff in
  let nperchunk = traflength / nchunks
  and lastchunk = traflength mod nchunks in
  write_partial_scens mod2traff nchunks nperchunk lastchunk
