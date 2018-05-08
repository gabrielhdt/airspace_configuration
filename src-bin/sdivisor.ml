let () =
  let nchunks = int_of_string Sys.argv.(2)
  and basefname = Sys.argv.(1) in
  Scendiv.divide basefname nchunks
