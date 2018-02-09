let () =
  let scpath = Sys.argv.(1) in
  let sc = Scenario.load scpath in

  let f = Scenario.f sc in

  let s01 = Util.Sset.add "3" (Util.Sset.add "2"
      (Util.Sset.add "1" Util.Sset.empty)) in
  let s02 = Util.Sset.add "5"
      (Util.Sset.add "4" Util.Sset.empty) in
  let conf0 = Airconf.make_conf 0 [(s01, ["a"]); (s02, ["b"])] in

  let s11 = Util.Sset.add "1" (Util.Sset.add "2" Util.Sset.empty) in
  let s12 = Util.Sset.add "3" (Util.Sset.add "4"
      (Util.Sset.add "5" Util.Sset.empty)) in
  let conf1 = Airconf.make_conf 1 [(s11, ["c"]); (s12, ["d"])] in

  let s21 = Util.Sset.add "1" (Util.Sset.add "2" Util.Sset.empty) in
  let s22 = Util.Sset.add "3" (Util.Sset.add "4"
      (Util.Sset.add "5" Util.Sset.empty)) in
  let conf2 = Airconf.make_conf 2 [(s21, ["c"]); (s22, ["d"])] in

  let path = [conf0; conf1; conf2] in

  let c = Airconf.path_cost f path Airconf.conf_cost Airconf.trans_cost in
  Printf.printf "%f\n" c;
  ()
