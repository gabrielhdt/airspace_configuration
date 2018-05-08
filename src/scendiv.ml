open Yojson.Basic.Util
open Printf

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

let write_partial_scens data ndiv cardperdiv nlastdiv basefname =
  let rec loop k = (* Iteration over the file number *)
    if k >= ndiv then () else
      let fname = basefname ^ (string_of_int k) in
      let sliced = List.map (fun (em, traf) ->
          let ind_begin, ind_end = k * cardperdiv, (k + 1) * cardperdiv in
          (em, slice ind_begin ind_end traf)) data in
      (* Build the string to be written to file *)
      (* slem2traf --> [ ("OY", [13, ..., 12]) ; ... ] *)
      let slem2traf = List.map (fun (em, traf) ->
          (em, String.concat ", " (List.map string_of_int traf))) sliced in
      (* slemtraf --> [ "OY: [13, ..., 12]" ; ... ] *)
      let slemtraf = List.map (fun (em, traf) -> sprintf "\"%s\": [%s]" em traf)
          slem2traf in
      (* The final file without opening curly brackets *)
      let scorpus = String.concat ",\n\t" slemtraf in
      let sfinal = sprintf "{\n\t%s\n}" scorpus
      (* Now write the string to file *)
      and outfile = open_out fname in
      begin
        fprintf outfile "%s" sfinal;
        close_out outfile;
        (* And loop *)
        loop (k + 1)
      end
  in
  loop 0

let divide fpath ndivs =
  let scenjson = Yojson.Basic.from_file Sys.argv.(1) in
  let mod2json = to_assoc scenjson in
  let mod2traff = List.map (fun (sec, json) ->
      (sec, List.map to_int @@ to_list json)) mod2json in
  let traflength = List.length @@ snd @@ List.hd mod2traff in
  let nperchunk = traflength / ndivs
  and lastchunk = traflength mod ndivs in
  write_partial_scens mod2traff ndivs nperchunk lastchunk fpath
