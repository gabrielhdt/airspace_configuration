let _cnt = ref 0;;

Arg.parse Options.speclist Options.anon_fun (Options.usage Sys.argv.(0)) ;;
let sc = Scenario.load !Options.scpath
let s15 = Util.Sset.add "1" (Util.Sset.add "5" Util.Sset.empty)
let s32 = Util.Sset.add "3" (Util.Sset.add "2" Util.Sset.empty)
let s4 = Util.Sset.add "4" Util.Sset.empty
let initial_partition = [
  (s15, [("d") ]) ; (s32, [("a") ]) ; (s4, [("s4") ])
]

module Env = struct
  let tmax = !Options.maxsearch
  let alpha = !Options.alpha
  let beta = !Options.beta
  let gamma = !Options.gamma
  let lambda = !Options.lambda
  let theta = !Options.theta
  let init = initial_partition
  let workload = Scenario.workload sc
end

module MctsParam = struct
  let expvexp = !Options.expvexp
end

module Support = Airconf.Make(Env)

type 'a tree =
  | Leaf
  | Node of 'a * (('a tree) list)

module NSet = Set.Make (
  struct
    type t = Support.t tree
    let compare = compare
  end)


let iter_tree t f_leaf f_node=
  let rec inner node =
    match node with
    | Leaf -> f_leaf ()
    | Node (v, l) -> f_node v l; List.iter inner l
  in
  inner t

let fold_tree f t (accu : NSet.t) =
  let rec inner (node : Support.t tree) (a : NSet.t) =
    match node with
    | Leaf -> a
    | Node (conf, children) ->
      let newacc = List.fold_left (fun acc elt ->
        f elt acc) a children in
      List.fold_left (fun acc elt -> inner elt acc)
        (newacc : NSet.t) (children : Support.t tree list)
  in
  inner t accu

let build_tree root produce term =
  let rec inner node =
    if term node
    then Leaf
    else Node (node, List.map (fun e -> inner e) (produce node))
  in
  inner root

let len tree =
  let rec inner tree accu =
    match tree with
    | Leaf -> accu
    | Node (_, l) -> inner (List.hd l) (accu + 1)
  in
  inner tree 0

let find_best htbl =
  Hashtbl.fold (fun k v (a,b) ->
      if v < b then (k,v) else (a,b)
    ) htbl (Leaf, infinity)


let best_path data node =
  let rec inner node accu =
    let prev = match node with
      | Leaf -> failwith "not good"
      | Node (n, _) ->
        snd (Hashtbl.find data n) in
    match prev with
    | None -> accu
    | Some n -> inner n (node::accu)
  in
  inner node []

exception Eureka

let dijkstra t =
  let root = match t with
  | Leaf -> failwith "pas de branche"
  | Node (conf, sons) -> conf in
  let data = Hashtbl.create (len t) in
  iter_tree t (fun _ -> ()) (fun n _ -> Hashtbl.add data n (infinity, None));
  Printf.printf "data filled%!\n" ;
  Hashtbl.replace data root (0., None);
  let q = ref (fold_tree (fun a accu -> NSet.add a accu) t NSet.empty) in
  Printf.printf "Q set up%!\n" ; let path = ref [] in

  try
    while NSet.cardinal !q <> 0 do
      incr _cnt ; Printf.printf "\r%d%!" !_cnt ;
      let (u, value) = NSet.fold (fun elt (k, v) ->
          match elt with
          | Leaf -> path := best_path data elt; raise Eureka
          | Node (conf, _) ->
            let (new_v, _) = (Hashtbl.find data conf) in
            if new_v < v then (elt, new_v) else (k, v)
        ) !q (t, infinity) in
      match u with
      | Leaf -> path := best_path data u ; raise Eureka
      | Node (v, l) -> if fst (Hashtbl.find data v) = infinity
          then failwith "tata"
          else
            begin
              q := NSet.remove u !q;
              List.iter (fun s ->
                  match s with
                  | Leaf -> ()
                  | Node (v, _) ->
                    let alt = value +. Support.cost v in
                    if alt < fst (Hashtbl.find data v) then
                      Hashtbl.replace data v (alt, Some u)
                ) l;
            end
    done;
    failwith "Unreachable"
  with Eureka -> !path


let () =
  let root = Support.init in
  let my_tree = build_tree root Support.produce Support.terminal in
  Printf.printf "%d\n%!" (len my_tree) ;
  ignore @@ dijkstra my_tree
