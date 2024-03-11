let taille_wall = 200
let taille_zone = 100


let red = Texture.color (Gfx.color 255 0 0 255)
let black = Texture.color (Gfx.color 0 0 0 255)
let violet = Texture.color (Gfx.color 102 0 102 255)
let trans_blue = Texture.color (Gfx.color 0 0 255 128)
let trans_red = Texture.color (Gfx.color 255 0 0 128)
let load_text = true


let new_line file i = 
  match file with 
  a::l -> l,a 
  |_ -> failwith (Printf.sprintf"Fichier fini ligne : %d" i )

let next_carac line i =
  match line with
  a::l -> l,a
  |_ -> Gfx.debug "Pb ligne n°%n du fichier de level \n%!" i;
  failwith (Printf.sprintf "Pb ligne n° %n du fichier de level" i)

let int_carac line i =
  let l,a = next_carac line i in 
  l, int_of_string a

let rec load_wall murs x_mur y_mur ligne colonne =
  match murs with 
  [] -> ()
  |mur :: murs -> 
    let () = match mur with 
      "h" -> 
        ignore(Wall.create (Printf.sprintf "mur_haut_%n:%n" x_mur y_mur) x_mur y_mur 200 10 black);
      |"b" -> let y_mur = y_mur + 190 in 
        ignore(Wall.create (Printf.sprintf "mur_bas_%n:%n" x_mur y_mur) x_mur y_mur 200 10 black);
      |"g" -> 
        ignore(Wall.create (Printf.sprintf "mur_gauche_%n:%n" x_mur y_mur) x_mur y_mur 10 200 black);
      |"d" -> let x_mur = x_mur + 190 in 
        ignore(Wall.create (Printf.sprintf "mur_droite_%n:%n" x_mur y_mur) x_mur y_mur 10 200 black);
      |"0" -> ()
      |_ -> Gfx.debug "Mur mal créé ligne n°%n colonne n°%n \n%!" ligne colonne
      in
    load_wall murs x_mur y_mur ligne colonne


    
let rec load_colonne ligne_murs y_mur x_lab ligne colonne =
  (**
      [load_colonne ligne_murs x_mur x_lab y_lab ligne colonne]*)
  if ligne_murs = [] then () 
  else begin
    let x_mur = colonne * taille_wall + x_lab in 
    let ligne_murs, mur = next_carac ligne_murs (3+ ligne) in
    let murs = String.split_on_char ';' mur in 
    (*les 3 carac d'une case*)
    load_wall murs x_mur y_mur ligne colonne;
    load_colonne ligne_murs y_mur x_lab ligne (colonne+1)
  end 
  

let rec load_walls line file x_lab y_lab ligne =
  (**
      [load_walls line file x_lab y_lab ligne]*)
  if line = "" then file 
  else begin 
  let y_mur = ligne * taille_wall + y_lab in 
  let ligne_murs = String.split_on_char  ' ' line in 
  load_colonne ligne_murs y_mur x_lab ligne 0;
  let file, line = new_line file 4 in 
  load_walls line file x_lab y_lab (ligne+1)
  end


let rec load_moov_zones dir_zone haut bas gauche droite x_zone y_zone=
  if dir_zone = [] then haut,bas,gauche, droite 
  else begin 
    let dir_zone,a = next_carac dir_zone 7 in 
    match a with 
    "h" -> load_moov_zones dir_zone true bas gauche droite x_zone y_zone
    |"b"-> load_moov_zones dir_zone haut true gauche droite x_zone y_zone
    |"g"-> load_moov_zones dir_zone haut bas true droite x_zone y_zone
    |"d"-> load_moov_zones dir_zone haut bas gauche true x_zone y_zone
    |_ -> Gfx.debug "Mauvaise direction enregistrée sur la case %n:%n" x_zone y_zone;
    failwith (Printf.sprintf "Mauvaise direction enregistrée sur la case %n:%n" x_zone y_zone)
  end

let lil_zone lil_x_zone lil_y_zone type_zone case = 
  match type_zone with 
    "1" ->  let case, dir_zone = next_carac case 7 in 
            let dir_zone = String.split_on_char ';' dir_zone in 
            let haut,bas,gauche,droite = load_moov_zones dir_zone false false false false lil_x_zone lil_y_zone in 
            let text = if load_text then (Texture_manager.load_texture_img haut bas gauche droite) else violet in  
            ignore(Zone.create_moov (Printf.sprintf "Zone_Moov_%n:%n" lil_x_zone lil_y_zone) lil_x_zone lil_y_zone (taille_zone/2) (taille_zone/2) text haut bas gauche droite);
            case
    
    |"2" -> ignore(Zone.create_death (Printf.sprintf "Zone_Death_%n:%n" lil_x_zone lil_y_zone) lil_x_zone lil_y_zone (taille_zone/2) (taille_zone/2));
            case
    
    |"3" -> let case, sibling = next_carac case 7 in
            ignore(Zone.create_tp_entree (Printf.sprintf "Tp_enter_%n:%n" lil_x_zone lil_y_zone) sibling lil_x_zone lil_y_zone (taille_zone/2) (taille_zone/2));
            case
    
    |"4" -> let case, sibling = next_carac case 7 in 
            ignore(Zone.create sibling lil_x_zone lil_y_zone (taille_zone/2) (taille_zone/2) 4);
            case
    |_ -> case




let rec load_zone line file x_lab y_lab =
  (**
      [load_zone line file x_lab y_lab]*)
  if line = "" then file 
  else begin 
    let case = String.split_on_char ' ' line in 
    let case, ligne = int_carac case 7 in 
    let x_zone = ligne * taille_zone + x_lab in
    let case, colonne = int_carac case 7 in 
    let y_zone = colonne * taille_zone + y_lab in 
    let case, type_zone = next_carac case 7 in 
    let () = match type_zone with 
    "1" ->  let case, dir_zone = next_carac case 7 in 
            let dir_zone = String.split_on_char ';' dir_zone in 
            let haut,bas,gauche,droite = load_moov_zones dir_zone false false false false x_lab y_lab in 
            let text = try if load_text then (Texture_manager.load_texture_img haut bas gauche droite) else violet 
                       with e ->begin 
                        let error = Printexc.to_string e in Gfx.debug "%s \n%!" error;
                        failwith (error) end in  
            ignore(Zone.create_moov (Printf.sprintf "Zone_Moov_%n:%n" x_zone y_zone) x_zone y_zone taille_zone taille_zone text haut bas gauche droite)
    
    |"2" -> ignore(Zone.create_death (Printf.sprintf "Zone_Death_%n:%n" x_zone y_zone) x_zone y_zone taille_zone taille_zone)
    
    |"3" -> let case, sibling = next_carac case 7 in
            ignore(Zone.create_tp_entree (Printf.sprintf "Tp_enter_%n:%n" x_zone y_zone) sibling x_zone y_zone taille_zone taille_zone)
    
    |"4" -> let case, sibling = next_carac case 7 in 
            ignore(Zone.create sibling x_zone y_zone taille_zone taille_zone 4)

    |"6" -> ignore(Zone.create (Printf.sprintf "Victoire_%n:%n" x_zone y_zone) x_zone y_zone taille_zone taille_zone 6)
    
    |"S" -> let case, deux_points = next_carac case 7 in 
            let lil_x_zone = x_zone in 
            let lil_y_zone = y_zone in 
            (*haut gauche*)
            let case, type_zone = next_carac case 7 in
            let case = lil_zone lil_x_zone lil_y_zone type_zone case in
            (*haut droite*)
            let case, type_zone = next_carac case 7 in
            let lil_x_zone = x_zone + 50 in 
            let case = lil_zone lil_x_zone lil_y_zone type_zone case in
            (*bas gauche*)
            let case, type_zone = next_carac case 7 in
            let lil_x_zone = x_zone in 
            let lil_y_zone = y_zone + 50 in 
            let case = lil_zone lil_x_zone lil_y_zone type_zone case in
            (*bas droite*)
            let case, type_zone = next_carac case 7 in
            let lil_x_zone = x_zone + 50 in 
            ignore(lil_zone lil_x_zone lil_y_zone type_zone case)
            

    |_ -> 
      Gfx.debug "ligne buggée : %s\n%!" line
  in
  let file, line = new_line file 6 in 
  load_zone line file x_lab y_lab
  end




let load_lvl lvl =
  let file = Level_manager.create_file lvl in 
  (*Ligne 1 joueur*)
  let file = String.split_on_char '\n' file in 
  let file, line = new_line file 1 in 
  (*let joueur = String.split_on_char ' ' line in
  let joueur, x_player = int_carac joueur 1 in
  let joueur, y_player = int_carac joueur 1 in
  let player = Player.create "player" x_player y_player 10 10 red in 
  (* les directions sont pas encore prises en compte*)
  let camera = Camera.create "camera" 0 0 800 600 in *)
  
  (*Ligne 2 labyrinthe*)
  let file, line = new_line file 2 in 
  let lab = String.split_on_char ' ' line in 
  
  let lab, x_lab = int_carac lab 2 in 
  let lab, y_lab = int_carac lab 3 in 
  (*Ligne 3*)
  let file, line = new_line file 3 in 
  (*Ligne 4 murs*)
  let file, line = new_line file 4 in 
  let file = load_walls line file x_lab y_lab 0 in 


  (*ligne 5*)
  (*ligne 6*)
  let file, line = new_line file 6 in 

  (*ligne 7 : les zones*)
  let file, line = new_line file 7 in 
  ignore(load_zone line file x_lab y_lab);  
  (*player, camera*)





