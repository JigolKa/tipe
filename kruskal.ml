(* implémentation Union-find *)
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


(* implémentation tri fusion *)

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

(* implémentation de kruskal *)
type graphe = (int*int) list array (* (voisin, coût) *)

let kruskal (g: graphe): (int*int*int) list =
    let n = Array.length g in
    let aretes = ref [] in

    for i=0 to n-1 do
        List.iter (fun (nbr, c) -> aretes := (i, nbr, c)::!aretes) g.(i)
    done;
    aretes := tri_fusion (!aretes) (fun (a,b,c) -> c);
    let subset = ref [] in
    let parents = ref (init_union_find n) in
    List.iter (fun (u,v, cost) -> 
        if find_parent !parents u != find_parent !parents v then
            begin
            subset := (u,v,cost)::!subset;
            parents := merge_classes !parents u v
            end
        ) !aretes; !subset

let g : graphe = [|
  [(1,4); (2,3); (3,8)];          (* 0 *)
  [(0,4); (2,1); (4,5); (5,7)];   (* 1 *)
  [(0,3); (1,1); (3,2); (5,6)];   (* 2 *)
  [(0,8); (2,2); (4,3); (6,4)];   (* 3 *)
  [(1,5); (3,3); (5,2); (7,6)];   (* 4 *)
  [(1,7); (2,6); (4,2); (6,1)];   (* 5 *)
  [(3,4); (5,1); (7,3)];          (* 6 *)
  [(4,6); (6,3)]                  (* 7 *)
|]


let graphe_tsp : (int * int) list array = [|
  [(1, 7); (2, 9); (3, 8); (4, 12); (5, 5)];   (* 0 *)
  [(0, 7); (2, 6); (3, 10); (4, 4); (5, 11)];  (* 1 *)
  [(0, 9); (1, 6); (3, 3); (4, 8); (5, 13)];   (* 2 *)
  [(0, 8); (1, 10); (2, 3); (4, 7); (5, 9)];   (* 3 *)
  [(0, 12); (1, 4); (2, 8); (3, 7); (5, 6)];   (* 4 *)
  [(0, 5); (1, 11); (2, 13); (3, 9); (4, 6)];  (* 5 *)
|]
