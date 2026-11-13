(* type graphe = (int*int) list array (voisin, coût) *)
type graphe_complet = int array array
type chaine = int array
type cell = int*int


let () = Random.self_init ()

let somme_chaine (g:graphe_complet) (chaine: chaine) : int =
  let ans = ref 0 in
  let n = Array.length chaine in
  for i=0 to n-2 do
    ans := !ans + g.(chaine.(i)).(chaine.(i+1))
  done;
  !ans + g.(chaine.(n-1)).(chaine.(0))



(* --- UNION-FIND --- *)
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
(* --- UNION-FIND --- *)



(* --- TRI FUSION --- *)
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
(* --- TRI FUSION --- *)


(* --- KRUSKAL --- *)
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
(* --- KRUSKAL --- *)

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
            (* print_chaine chaine; *)
            nouvelle_chaine i j;
            (* print_chaine chaine; *)

            found := true
          end
      done
    done;
  done; chaine

let approximation2opt_greedy (g: graphe_complet) : chaine =
  let chaine = (greedy_TSP g 0) in
  approximation2opt_blueprint g chaine

let greedy_random (g: graphe_complet) (ntest: int) : float =
  let sum = ref 0. in
  for i=0 to ntest-1 do
    let s = Random.int_in_range ~min:0 ~max:(Array.length g -1) in
    let ans = greedy_TSP g s in
    sum := !sum +. (float_of_int (somme_chaine g ans))
  done; !sum /. (float_of_int ntest)


(* TODO: random start? *)
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
            let set_without_i = bitset lxor pow.(i) in
            for m=1 to n-1 do
              if i <> m && (set_without_i land pow.(m) = pow.(m)) then
                begin
                  mini := min !mini (g.(set_without_i).(m) + graph.(m).(i))
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





(* show_diff renvoie 2 valeurs différentes pour graphe_diff *)
let show_diff (g: graphe_complet) : unit = 
  let a = approximation2opt_ACM g in
  let b = approximation2opt_greedy g in
  Printf.printf "Approximation depuis glouton: %d\n" (somme_chaine g a);
  Printf.printf "Approximation depuis ACM: %d\n" (somme_chaine g b);
  Printf.printf "Glouton aléatoire avec %d tests: %.2f\n" 1000 (greedy_random g 1000);
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

let graphe_15 : graphe_complet = [|
  [|  0; 16; 41; 65; 66; 83; 14; 29; 77; 80; 72; 54; 74; 71; 94 |];
  [| 16;  0; 99; 63; 97; 99; 76; 57; 31;  1; 79; 11; 15; 37; 13 |];
  [| 41; 99;  0; 58;  2; 88; 63; 87; 41; 27; 51; 33; 45; 46; 49 |];
  [| 65; 63; 58;  0; 96; 66; 82; 10; 93; 44; 12; 72; 69; 38; 37 |];
  [| 66; 97;  2; 96;  0; 59; 19; 83; 91; 92; 74; 40;  4; 91; 48 |];
  [| 83; 99; 88; 66; 59;  0; 47; 60; 55; 12; 52; 75; 71; 64; 15 |];
  [| 14; 76; 63; 82; 19; 47;  0; 55; 65; 97; 77; 63; 51; 68; 34 |];
  [| 29; 57; 87; 10; 83; 60; 55;  0; 54; 73; 62; 66; 67;  4; 74 |];
  [| 77; 31; 41; 93; 91; 55; 65; 54;  0; 29; 99; 17;  7; 91; 67 |];
  [| 80;  1; 27; 44; 92; 12; 97; 73; 29;  0; 86; 14; 92; 80; 55 |];
  [| 72; 79; 51; 12; 74; 52; 77; 62; 99; 86;  0; 61; 19; 62; 30 |];
  [| 54; 11; 33; 72; 40; 75; 63; 66; 17; 14; 61;  0; 16; 81; 90 |];
  [| 74; 15; 45; 69;  4; 71; 51; 67;  7; 92; 19; 16;  0; 59; 87 |];
  [| 71; 37; 46; 38; 91; 64; 68;  4; 91; 80; 62; 81; 59;  0; 61 |];
  [| 94; 13; 49; 37; 48; 15; 34; 74; 67; 55; 30; 90; 87; 61;  0 |];
|]



(* --- SIMPLEX --- *)

(* https://medium.com/@muditbits/simplex-method-for-linear-programming-1f88fc981f50 *)

type constraint_matrix = int array array
type augmented_matrix = int array array
type 'a matrix = 'a array array

let print_matrix (a: 'a array array) : unit =
  let n =  Array.length a in
  let m  = Array.length a.(0) in
  for i=0 to n-1 do
    for j=0 to m-1 do
      Printf.printf "%d " a.(i).(j)
    done;
    Printf.printf "\n"
  done

(* ajoute l'identité à droite ie renvoie [ a | I_m ] *)
let augment_matrix (a: constraint_matrix) : augmented_matrix =
  let m = Array.length a in (* number of inequalities *)
  let n  = Array.length a.(0) in (* number of variables *)
  let ans  = Array.make_matrix m (n+m) 0 in
  for i=0 to m-1 do
    for j=0 to n-1 do
      ans.(i).(j) <- a.(i).(j)
    done;
    ans.(i).(n+i) <- 1
  done; ans

let test_augment_matrix () = 
  let test = [|
    [|0;4;2|];
    [|3;2;1|]
  |] in
  print_matrix (augment_matrix test)


(* renvoie l'indice de la variable entrante et sortante *)
let find_entering_exiting_indices (a: int matrix) (rhs: int array) (objective_row: int array) : int*int =
  let m = Array.length a in
  let n = Array.length a.(0) in
  assert(Array.length objective_row = n);
  assert(Array.length rhs = m);
  let entering_mini = ref Int.max_int in
  let entering_idx = ref (-1) in (* column *)
  for i=0 to n-1 do
    if objective_row.(i) < !entering_mini then
      (entering_idx := i; entering_mini := objective_row.(i))
  done;
  let exiting_mini = ref Float.max_float in
  let exiting_idx = ref (-1) in
  for i=0 to m-1 do
    let candidate = (float_of_int rhs.(i)) /. (float_of_int a.(i).(!entering_idx)) in
    if candidate < !exiting_mini then
      (exiting_idx:=i;exiting_mini:=candidate)
  done; (!entering_idx, !exiting_idx)

(* let gauss_jordan  *)