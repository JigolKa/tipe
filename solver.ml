type noeud = {
  mutable chemin : int list;
  mutable heuristique: float;
  mutable cout_actuel:float;
}

type tas_min = {
    mutable taille : int;
    mutable capacite :int;
    mutable elements: noeud array;

}

let create_noeud (chemin_p :int list)(h:float)(c:float):noeud= {chemin = chemin_p ;heuristique = h;cout_actuel = c}
let creer_tas (capacite_s:int) =
  let noeud_vide = create_noeud [] 0.0 0.0 in
  let tas = { taille = 0;
                capacite=capacite_s;
                elements = Array.make capacite_s noeud_vide  } in
  tas

let redim_tas (tas:tas_min):unit =
  let new_capa = tas.capacite *2 in 
  let new_elem_tab = Array.make new_capa tas.elements.(0) in
  
  (*Array.blit permet de copie le contenu d'un tableau plus petit ou de meme taille vers un autre *)
  (* tableau_source -> indice_depart_source -> tableau_destination -> indice_depart_destination -> nombre_d_elements_à_copier *)
  Array.blit tas.elements 0 new_elem_tab 0 tas.taille;
  
  (*Modification des paramètres du tas avec le snouveaux elemnt et la nouvelle capacité *)
  tas.capacite <- new_capa;
  tas.elements <- new_elem_tab

(*Fonctions utiles pour accéder au fils droit,parent et fils gauche dan sun tas binaire (à savoir démontrer aussi gros )*)
let parent i = (i-1) / 2
let fils_gauche i = 2*i + 1
let fils_droit i = 2*i + 2

let echanger(tas:tas_min)(i:int)(j:int):unit=
  let temp = tas.elements.(i) in 
  tas.elements.(i) <- tas.elements.(j);
  tas.elements.(j) <- temp


let min (a:int)(b:int):int = 
  if a<b then 
    a 
  else 
    b

let rec percolation_ascendante (tas:tas_min)(i:int):unit = 
  if i=0 then
    ()
   else if tas.elements.(i).cout_actuel < tas.elements.(parent i).cout_actuel then begin
    echanger tas i (parent i);
    percolation_ascendante tas (parent i)
  end
  else 
    ()
let rec percolation_descendante (tas:tas_min)(i:int):unit = 
  let g = fils_gauche i in
  let d = fils_droit i in 
  (* On cherche le plus petit entre le parent i et le fils gauche g s'il existe *)
  let min_g = 
    if g < tas.taille && tas.elements.(g).cout_actuel < tas.elements.(i).cout_actuel then 
      g 
    else 
      i 
  in
  (*  On compare ce résultat avec le fils droit d s'il existe *)
  let min_id = 
    if d < tas.taille && tas.elements.(d).cout_actuel < tas.elements.(min_g).cout_actuel then 
      d 
    else 
      min_g 
  in 
  (* Si le parent n'est pas le plus petit, on échange et on continue *)
  if min_id <> i then begin
    echanger tas i min_id;
    percolation_descendante tas min_id
  end

let pop (tas:tas_min):noeud = 
  if tas.taille =0 then failwith "Erreur ,le tas est vide" ;
  let min = tas.elements.(0) in 
  tas.elements.(0) <- tas.elements.(tas.taille -1);
  tas.elements.(tas.taille -1) <-create_noeud [] 0.0 0.0;
  tas.taille <- tas.taille - 1;
  percolation_descendante tas 0;
  min
let push (tas:tas_min)(new_noeud :noeud):unit=
  if tas.taille = tas.capacite then
    redim_tas tas;
  tas.elements.(tas.taille) <-new_noeud;
  tas.taille <-tas.taille +1;
  percolation_ascendante tas (tas.taille-1) (*On remonte depuis l'ancien dernier  indice ie tas.taille -1*)


 let rec affiche_tas (tas:tas_min)(i:int)(indentation:string):unit = 
  if i < tas.taille then begin
    affiche_tas tas (fils_droit i) (indentation ^ "    ");
    
    Printf.printf "%s|-- %.1f\n" indentation tas.elements.(i).cout_actuel;
    
    affiche_tas tas (fils_gauche i) (indentation ^ "    ")
  end 

let charger_matrice (chemin : string) : float array array =
  (*On stocke le fichier ouvert*)
  let ic = open_in chemin in
  let rec boucle acc =
    match input_line ic with
    | ligne ->
        
        let texte_valeurs = String.split_on_char ',' (String.trim ligne) in
        let ligne_float = Array.of_list (List.map float_of_string texte_valeurs) in
        boucle (ligne_float :: acc)
    | exception End_of_file ->
        close_in ic;
        Array.of_list (List.rev acc)
  in
  boucle []





let mat_cout = charger_matrice "matrice_cout.txt" 

()
(* let solver_by_branch_and_bound  (mat_cout_dv :float array array)(nb_debris : int):int list =
  let meilleures_solutions = ref [] in
  let meillleur_cout_debris = 0.0 in 
  let todo = creer_tas nb_debris in
  let noeud_e = create_noeud [] 0.0 0.0 in
  
  let best_dv = 0 in  
  push todo noeud_e ;
  while todo.taille>0 do
    Printf.printf "hello"rf
  done
 *)

let tas = creer_tas 4 
let noeud1 = {chemin = [];cout_actuel=40.3;heuristique = 0.0} 
let noeud2 = {chemin = [];cout_actuel=60.2;heuristique = 0.0} 
let noeud3 = {chemin = [];cout_actuel=8.2;heuristique = 0.0} 
let noeud4 =  {chemin = [];cout_actuel=87.2;heuristique = 0.0} 


let () = 
  push tas noeud1;
  push tas noeud2;
  push tas noeud3;
  push tas noeud4;

  affiche_tas tas 0 " " ;
  pop tas ;
  print_newline();
  affiche_tas tas 0 " ";
  print_newline()

  


