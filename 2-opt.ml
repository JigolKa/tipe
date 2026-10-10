type graphe_complet = int array array
type chaine = int array

let graphe_tsp : graphe_complet = [|
  [| 0;  7;  9;  8; 12;  5|];
  [| 7;  0;  6; 10;  4; 11|];
  [| 9;  6;  0;  3;  8; 13|];
  [| 8; 10;  3;  0;  7;  9|];
  [|12;  4;  8;  7;  0;  6|];
  [| 5; 11; 13;  9;  6;  0|]
|]

type cell = int*int
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

let approximation2opt (g: graphe_complet) : chaine =
  let chaine = (greedy_TSP g 0) in
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
            Printf.printf "chemin pris: %d %d %d\n" i j delta_path;
            print_chaine chaine;
            nouvelle_chaine i j;
            print_chaine chaine;

            found := true
          end
      done
    done;
  done; chaine


let somme_chaine (g:graphe_complet) (chaine: chaine) : int =
  let ans = ref 0 in
  let n = Array.length chaine in
  for i=0 to n-2 do
    ans := !ans + g.(chaine.(i)).(chaine.(i+1))
  done;
  !ans + g.(chaine.(n-1)).(chaine.(0))

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