(* type graphe = (int*int) list array (voisin, coût) *)
type graphe_complet = int array array
type chaine = int array
type cell = int*int



(* DEBUT UNION-FIND *)
type uf = int array (* parent *)
let init_union_find (n: int) : uf =
    Array.init n (fun i -> i)

let rec find_parent (u: uf) (i: int) : int =
    if u.(i) = i then i
    else
        begin
            let ans = find_parent u (u.(i)) in
            u.(i)<-ans;
            ans
        end

let merge_classes (u: uf) (a: int) (b: int) : uf =
    let x = find_parent u a in
    let y = find_parent u b in
    u.(x)<-y;u
(* FIN UNION-FIND *)

(* DEBUT TRI FUSION *)
let t = [9;2;4;1;7;0;3;19;90;-3]

let rec merge_lists (l1: 'a list) (l2: 'a list) (comp: 'a -> int) : 'a list =
    match (l1, l2) with
    | [],[] -> []
    | a::q, []
    | [], a::q -> a::q
    | x::q, y::q' ->
        if comp x > comp y then y::merge_lists l1 q' comp
        else x::merge_lists q l2 comp

let rec tri_fusion (a: 'a list) (comp: 'a -> int) : 'a list =
    let n = List.length a in
    if n <= 1 then a
    else
        begin
            let mid = n/2 in
            let left_sub = List.filteri (fun i _ -> i < mid) a in
            let right_sub = List.filteri (fun i _ -> i >= mid) a in
            let left = tri_fusion left_sub comp in
            let right = tri_fusion right_sub comp in
            merge_lists left right comp
        end
(* FIN TRI FUSION *)


(* DEBUT KRUSKAL *)
let kruskal (g: graphe_complet): (int*int) list array =
    let n = Array.length g in
    let aretes = ref [] in

    for i=0 to n-1 do
      for j=i+1 to n-1 do
        aretes := (i,j,g.(i).(j))::!aretes
      done
    done;
    aretes := tri_fusion (!aretes) (fun (a,b,c) -> c);
    let acm = Array.make n [] in
    let parents = ref (init_union_find n) in
    List.iter (fun (u,v, cost) -> 
        if find_parent !parents u != find_parent !parents v then
            begin
            acm.(v) <- (u,cost)::acm.(v);
            acm.(u) <- (v,cost)::acm.(u);
            parents := merge_classes !parents u v
            end
        ) !aretes; acm


(* FIN KRUSKAL *)

let dist ((x,y): cell) ((x',y'): cell) : int =
  let dx = abs (x-x') in
  let dy = abs (y-y') in
  dx*dx+dy*dy

(* on prend le voisin le plus proche à chaque fois, graphe complet *)
(* renvoie l'ordre des sommets *)
let greedy_TSP (g: graphe_complet) (s: int) : chaine =
  let n = Array.length g in
  let traites = Array.make n false in
  let chaine = Array.make n (-1) in

  (* prend l'indice d'un sommet et renvoie les indices des voisins accessibles (non traités) *)
  let voisins_possibles (idx: int) : (int*int) list =
    let voisins = ref [] in
    for j=0 to n-1 do
      if idx<>j && not traites.(j) then
        voisins := (j, g.(idx).(j))::!voisins
    done; !voisins
    (* List.filter (fun (nbr_idx, _) -> not traites.(nbr_idx)) g.(idx) *)
  in

  let ptr = ref 0 in
  let rec expand_TSP_from (idx: int) : unit =
    traites.(idx) <- true;
    chaine.(!ptr) <- idx;
    incr ptr;
    let mini_dist = ref Int.max_int in
    let mini_idx = ref (-1) in
    List.iter (fun (nbr, cost) ->
      begin
        if not traites.(nbr) && cost < !mini_dist then
          (mini_dist := cost; mini_idx := nbr)
      end) (voisins_possibles idx);
    match !mini_idx with
    | -1 -> ()
    | next ->
      (expand_TSP_from (!mini_idx))

  in
  expand_TSP_from s;
  chaine

let print_chaine (c: chaine) : unit =
  let n = Array.length c in
  for i=0 to n-1 do
    Printf.printf "%d " c.(i)
  done;
  Printf.printf "\n"

let approximation2opt_blueprint (g: graphe_complet) (chaine: chaine) : chaine =
  let n = Array.length chaine in

  let nouvelle_chaine (v1: int) (v2: int) : unit =
    let i = ref (v1+1) in
    let j = ref v2 in
    while (!i < !j) do
      let tmp = chaine.(!i) in
      chaine.(!i) <- chaine.(!j);
      chaine.(!j) <- tmp;
      incr i;
      decr j;
    done
  in

  let found = ref true in
  while (!found) do
    found:=false;
    for i=0 to n-2 do
      for j=i+2 to n-1 do
        let delta_path = - g.(chaine.(i)).(chaine.(i+1)) - g.(chaine.(j)).(chaine.((j+1) mod n)) 
        + g.(chaine.(i)).(chaine.(j)) + g.(chaine.(i+1)).(chaine.((j+1) mod n)) in
        (* on prend le nouveau chemin *)
        if delta_path < 0 then
          begin
            (* Printf.printf "chemin pris: %d %d %d\n" i j delta_path; *)
            print_chaine chaine;
            nouvelle_chaine i j;
            print_chaine chaine;

            found := true
          end
      done
    done;
  done; chaine

let approximation2opt_greedy (g: graphe_complet) : chaine =
  let chaine = (greedy_TSP g 0) in
  approximation2opt_blueprint g chaine

let tri_prefixe_ACM (acm: (int*int) list array) : chaine =
  let n = Array.length acm in
  let vu = Array.make n false in
  let res = Array.make n (-1) in
  let ptr = ref 0 in
  vu.(0) <- true;
  let rec remplir_depuis (s: int) : unit =
    begin
      res.(!ptr) <- s;
      List.iter (fun (nbr, _) ->
          if not vu.(nbr) then
            (vu.(nbr)<-true; incr ptr; remplir_depuis nbr)
        ) acm.(s)
    end
  in
  remplir_depuis 0; res
  
let approximation2opt_ACM (g: graphe_complet) : chaine =
  let acm = kruskal g in
  let chaine = tri_prefixe_ACM acm in
  approximation2opt_blueprint g chaine

let remove_duplicates (l: 'a list) :'a list =
  let ans = ref [] in
  let n = List.length l in
  let tbl = Hashtbl.create n in
  List.iter (fun x ->
    if not (Hashtbl.mem tbl x) then
      begin
        Hashtbl.add tbl x true;
        ans := x::!ans
      end
    ) l;
    !ans


(* génère les nombres entre [0, 2^n -1] tq ils ont k bits = 1 *)
let rec generer_nombres (n: int) (k: int) : int list =
  if k = 0 then [0]
  else
    begin
      (* on ajoute un bit de plus *)
      let previous = generer_nombres n (k-1) in
      let next = ref [] in
      for i=0 to n-1 do
        let pos = 1 lsl i in
        List.iter (fun bitset ->
          begin
            let bit_already_set = bitset land pos = pos in
            if not bit_already_set then
              next := (bitset lor pos)::!next
          end
          ) previous
      done;
      remove_duplicates !next
    end

let held_karp (graph: graphe_complet) : int = 
  let n = Array.length graph in
  let g = Array.make_matrix (1 lsl n) n 0 in
  let pow = Array.make n 0 in
  pow.(0) <- 1;
  for i=1 to n-1 do
    pow.(i) <- 2*pow.(i-1);
    g.(pow.(i)).(i) <- graph.(0).(i)
  done;

  for s=2 to n-1 do
    List.iter (fun bitset -> (
      for i=1 to n-1 do
        if bitset land pow.(i) = pow.(i) then
          begin
            let mini = ref Int.max_int in
            let set_without_k = bitset lxor pow.(i) in
            for m=1 to n-1 do
              if i <> m && (set_without_k land pow.(m) = pow.(m)) then
                begin
                  mini := min !mini (g.(set_without_k).(m) + graph.(m).(i))
                end
            done;
            g.(bitset).(i) <- !mini
          end
      done;
    )) (generer_nombres n s)
  done;
  let mini = ref Int.max_int in
  let num = (1 lsl n) - 2 in (* {1,..,n-1} *)
  for k=1 to n-1 do
    mini := min !mini (g.(num).(k) + graph.(k).(0))
  done;
  !mini

let somme_chaine (g:graphe_complet) (chaine: chaine) : int =
  let ans = ref 0 in
  let n = Array.length chaine in
  for i=0 to n-2 do
    ans := !ans + g.(chaine.(i)).(chaine.(i+1))
  done;
  !ans + g.(chaine.(n-1)).(chaine.(0))

(* show_diff renvoie 2 valeurs différentes pour graphe_diff *)
let show_diff (g: graphe_complet) : unit = 
  let a = approximation2opt_ACM g in
  let b = approximation2opt_greedy g in
  Printf.printf "Approximation depuis glouton: %d\n" (somme_chaine g a);
  Printf.printf "Approximation depuis ACM: %d\n" (somme_chaine g b);
  Printf.printf "Valeur exacte Held-Karp: %d\n" (held_karp g)


(* EXEMPLES *)
let graphe_diff : graphe_complet = [|
  [| 0;  6;  8; 10; 11;  9|];
  [| 6;  0;  7;  1; 13; 12|];
  [| 8;  7;  0;  3;  4; 15|];
  [|10;  1;  3;  0;  2;  5|];
  [|11; 13;  4;  2;  0; 14|];
  [| 9; 12; 15;  5; 14;  0|]
|]

let piege : graphe_complet = [|
  [|  0;  1; 10; 10; 10; 100|];
  [|  1;  0;  1; 10; 10;  10|];
  [| 10;  1;  0;  1; 10;  10|];
  [| 10; 10;  1;  0;  1;  10|];
  [| 10; 10; 10;  1;  0;   1|];
  [|100; 10; 10; 10;  1;   0|]
|]

let graphe_tsp : graphe_complet = [|
  [| 0;  7;  9;  8; 12;  5|];
  [| 7;  0;  6; 10;  4; 11|];
  [| 9;  6;  0;  3;  8; 13|];
  [| 8; 10;  3;  0;  7;  9|];
  [|12;  4;  8;  7;  0;  6|];
  [| 5; 11; 13;  9;  6;  0|]
|]