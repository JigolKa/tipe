type noeud = {
  mutable chemin : int list;
  mutable heuristique: float;
  mutable cout_actuel:float;
  mutable taille_chemin:int;
  indice :int;
}

type tas_min = {
    mutable taille : int;
    mutable capacite :int;
    mutable elements: noeud array;

}

type graphe = {
    matrix : float array array;
    nb_sommet:int;
}

let create_noeud (chemin_p :int list)(h:float)(c:float)(i:int)(t_c:int):noeud= {taille_chemin =t_c ;chemin = chemin_p ;heuristique = h;cout_actuel = c;indice =i}
let creer_tas (capacite_s:int) =
  let noeud_vide = create_noeud [] 0.0 0.0 0 0 in
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
   else if tas.elements.(i).heuristique < tas.elements.(parent i).heuristique then begin
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
    if g < tas.taille && tas.elements.(g).heuristique < tas.elements.(i).heuristique then 
      g 
    else 
      i 
  in
  (*  On compare ce résultat avec le fils droit d s'il existe *)
  let min_id = 
    if d < tas.taille && tas.elements.(d).heuristique < tas.elements.(min_g).heuristique then 
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
  tas.elements.(tas.taille -1) <-create_noeud [] 0.0 0.0 0 0;
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


let prim_heuristique (g:graphe)(s:int):float= 
  (*------Création de tableau indéxé par le nombre de sommet de g--------*)
  (*On définit un tableau de cout *)
  let cout = Array.make g.nb_sommet infinity in
  (*Et un tableau de predecesseurs tous initialisés à None*)
  let pred = Array.make g.nb_sommet None in
  (*Et un tableau verifiant la visite ou non d'un sommet indéxé*)
  let visite = Array.make g.nb_sommet false in 
  (*---------------------------------------------------------------------*)
  let cout_total = ref 0.0 in
  cout.(s) <- 0.0;
  pred.(s)<-Some(s);
  let noeud_s = create_noeud [] 0.0 cout.(s) s 0 in
  let todo = creer_tas g.nb_sommet in
  push todo noeud_s;
  while todo.taille>0 do
    let u = pop todo in
    if not visite.(u.indice) then begin
      visite.(u.indice) <- true;
      cout_total := !cout_total +. cout.(u.indice);
      for i=0 to g.nb_sommet -1 do 
          if not visite.(i) && cout.(i) >= g.matrix.(u.indice).(i) then begin
            pred.(i)<-Some(u.indice);
            cout.(i)<-g.matrix.(u.indice).(i);
            let noued_i = create_noeud [] 0.0 cout.(i) i 0 in
            push todo noued_i
          end
      done
    end
  done;
  !cout_total



let solver_by_branch_and_bound  (g:graphe)(s:int)(nb_debris : int):int list =
  let meilleures_cout_global = ref infinity in
  let tous_debris = List.init nb_debris (fun i -> i) in
  let meillleur_chemin_debris = ref [] in 
  let todo = creer_tas nb_debris in
  let noeud_racine = create_noeud [s] (prim_heuristique g s) 0.0 s 1 in
  push todo noeud_racine ;
  while (todo.taille > 0) do
    let noeud_courant = pop todo in
    if noeud_courant.heuristique>= !meilleures_cout_global then begin
      ()
    end
    if noeud_courant.taille_chemin = g.nb_sommet then begin
      if noeud_courant.cout_actuel<!meilleures_cout_global then begin
        meilleures_cout_global := noeud_courant.cout_actuel;
        meillleur_chemin_debris:= noeud_courant.chemin;
      end
    end

    for debris_voisin=0 to g.nb_sommet-1 do 
      if not (List.mem i noeud_courant.chemin) then begin
        let nouveau_cout =noeud_courant.cout_actuel + g.matrix.(noeud.noeud_courant.indice).(debris_voisin) in
        if nouveau_cout >= !meilleures_cout_global then begin
          ()
        end
        let debris_restants = List.filter (fun x -> not (List.mem x noeud_courant.chemin)) tous_debris in
        let heuristique = prim_heuristique g debris_voisin in
        let nouvelle_borne = nouveau_cout + heuristique in 
        if nouvelle_borne<!meilleures_cout_global then begin
          let nouveau_noeud = create_noeud  debris_voisin :: noeud_courant.chemin in
          push todo nouveau_noeud;
        end
      

      end
    done
  done;
  (!meilleures_cout_global,!meilleures_cout_global)


let mat_cout = charger_matrice "matrice_cout.txt" 





let tas = creer_tas 4 
let noeud1 = create_noeud [] 0.0 40.3 1 
let noeud2 = create_noeud [] 0.0 60.2 1 
let noeud3 = create_noeud [] 0.0 86.2 1 
let noeud4 =  create_noeud [] 0.0 3.0 1 


let () = 
  push tas noeud1;
  push tas noeud2;
  push tas noeud3;
  push tas noeud4;

  affiche_tas tas 0 " " ;
  let noued_recup = pop tas in
  print_newline();
  affiche_tas tas 0 " ";
  print_newline()

  


