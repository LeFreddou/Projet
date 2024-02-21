open Component_def
open System_def

let taille_case = 200


let red = Texture.color (Gfx.color 255 0 0 255)
let black = Texture.color (Gfx.color 0 0 0 255)
let violet = Texture.color (Gfx.color 102 0 102 255)
let trans_blue = Texture.color (Gfx.color 0 0 255 128)
let trans_red = Texture.color (Gfx.color 255 0 0 128)

let new_line file i = 
  match file with 
  a::l -> l,a 
  |_ -> failwith "Fichier fini"

let next_carac line i =
  match line with
  a::l -> l,a
  |_ -> Gfx.debug "Pb ligne n°%n du fichier de level \n%!" i;
  failwith (Printf.sprintf "Pb ligne n° %n du fichier de level" i)

let int_carac line i =
  let l,a = next_carac line i in 
  l, int_of_string a  

let rec load_colonne ligne_murs x_mur x_lab y_lab ligne colonne =
  (**
      [load_colonne ligne_murs x_mur x_lab y_lab ligne colonne]*)
  if ligne_murs = [] then () 
  else begin
    let y_mur = colonne * taille_case + y_lab in 
    let ligne_murs, mur = next_carac ligne_murs (3+ ligne) in 
    let murs = String.split_on_char ';' mur in 
    (*les 3 carac d'une case*)
    for i = 1 to 3 do 
      let murs, mur = next_carac murs (3+ ligne) in 
      match mur with 
      "h" -> 
        ignore(Wall.create (Printf.sprintf "mur_haut_%n:%n" x_mur y_mur) x_mur y_mur 200 10 black) 
      |"b" -> let y_mur = y_mur + 190 in 
        ignore(Wall.create (Printf.sprintf "mur_bas_%n:%n" x_mur y_mur) x_mur y_mur 200 10 black)
      |"g" -> 
        ignore(Wall.create (Printf.sprintf "mur_gauche_%n:%n" x_mur y_mur) x_mur y_mur 10 200 black)
      |"d" -> let x_mur = x_mur + 190 in 
        ignore(Wall.create (Printf.sprintf "mur_droite_%n:%n" x_mur y_mur) x_mur y_mur 10 200 black)
      |"0" -> ()
      |_ -> Gfx.debug "Mur mal créé ligne n°%n colonne n°%n \n%!" ligne colonne;
        failwith (Printf.sprintf "Mur mal créé ligne n°%n colonne n°%n " ligne colonne ) 
      done;
    load_colonne ligne_murs x_mur x_lab y_lab ligne (colonne+1)
  end 
  

let rec load_walls line file x_lab y_lab ligne =
  (**
      [load_walls line file x_lab y_lab ligne]*)
  if line = "\n" then file 
  else begin 
  let x_mur = ligne * taille_case + x_lab in 
  let ligne_murs = String.split_on_char  ' ' line in 
  load_colonne ligne_murs x_mur x_lab y_lab ligne 0;
  let file, line = new_line file 4 in 
  load_walls line file x_lab y_lab (ligne+1)
  end


let rec load_moov_zones dir_zone haut bas gauche droite x_zone y_zone=
  if dir_zone = [] then haut,gauche,bas, droite 
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
            ignore(Zone.create_moov (Printf.sprintf "Zone_Moov_%n:%n" lil_x_zone lil_y_zone) lil_x_zone lil_y_zone 100 100 violet haut bas gauche droite);
            case
    
    |"2" -> ignore(Zone.create (Printf.sprintf "Zone_Death_%n:%n" lil_x_zone lil_y_zone) lil_x_zone lil_y_zone 100 100 2);
            case
    
    |"3" -> let case, sibling = next_carac case 7 in
            ignore(Zone.create_tp_entree (Printf.sprintf "Tp_enter_%n:%n" lil_x_zone lil_y_zone) sibling lil_x_zone lil_y_zone 100 100);
            case
    
    |"4" -> let case, sibling = next_carac case 7 in 
            ignore(Zone.create sibling lil_x_zone lil_y_zone 100 100 4);
            case
    |_ -> case


let rec load_zone line file x_lab y_lab =
  (**
      [load_zone line file x_lab y_lab]*)
  if line = "\n" then file 
  else begin 
    let case = String.split_on_char ' ' line in 
    let case, x_zone = int_carac case 7 in 
    let x_zone = x_zone * 100 - x_lab in
    let case, y_zone = int_carac case 7 in 
    let y_zone = y_zone * 100 - y_lab in 
    let case, type_zone = next_carac case 7 in 
    let () = match type_zone with 
    "1" ->  let case, dir_zone = next_carac case 7 in 
            let dir_zone = String.split_on_char ';' dir_zone in 
            let haut,bas,gauche,droite = load_moov_zones dir_zone false false false false x_lab y_lab in 
            ignore(Zone.create_moov (Printf.sprintf "Zone_Moov_%n:%n" x_zone y_zone) x_zone y_zone 100 100 violet haut bas gauche droite)
    
    |"2" -> ignore(Zone.create (Printf.sprintf "Zone_Death_%n:%n" x_zone y_zone) x_zone y_zone 100 100 2)
    
    |"3" -> let case, sibling = next_carac case 7 in
            ignore(Zone.create_tp_entree (Printf.sprintf "Tp_enter_%n:%n" x_zone y_zone) sibling x_zone y_zone 100 100)
    
    |"4" -> let case, sibling = next_carac case 7 in 
            ignore(Zone.create sibling x_zone y_zone 100 100 4)
    
    |"S" -> let case, deux_points = next_carac case 7 in 
            let lil_x_zone = x_zone in 
            let lil_y_zone = y_zone in 
            let case, type_zone = next_carac case 7 in
            let case = lil_zone lil_x_zone lil_y_zone type_zone case in
            let lil_x_zone = x_zone + 50 in 
            let case = lil_zone lil_x_zone lil_y_zone type_zone case in
            let lil_x_zone = x_zone in 
            let lil_y_zone = y_zone + 50 in 
            let case = lil_zone lil_x_zone lil_y_zone type_zone case in
            let lil_x_zone = x_zone + 50 in 
            ignore(lil_zone lil_x_zone lil_y_zone type_zone case)
            

    |_ -> ()
  in
  let file, line = new_line file 6 in 
  load_zone line file x_lab y_lab
  end

let level = ref None

let load_file dst path =
  dst := Some (Gfx.load_file path);
  Gfx.debug "ressource loadé\n%!"

let wait_file rsc _dt =
  match !rsc with
    None -> failwith "Error"
    | Some r -> not (Gfx.resource_ready r)


let create_file lvl = 
  let path = match lvl with
  0 -> "resources/files/test.level"
  |1 -> "resources/files/01.level"
  |2 -> "resources/files/02.level"
  |_ -> Gfx.debug "Pas de niveau \n%!";
        failwith "Pas de niveau"
  in
  Gfx.debug "Chemin trouvé\n%!";
  load_file level path;
  Gfx.main_loop (wait_file level);
  Gfx.debug "resource chargée\n%!";
  match !level with 
  None -> Gfx.debug "Pas de niveau \n%!";
  assert false
  |Some r -> try Gfx.get_resource r with e ->let error = Printexc.to_string e in Gfx.debug "Why %s \n%!" error;
  failwith "Je sais pas honnêtement"




let load_lvl lvl =
  Gfx.debug "début loading\n%!";
  let file = create_file lvl in 
  Gfx.debug "fichier loadé\n%!";
  (*Ligne 1 joueur*)
  let file = String.split_on_char '\n' file in 
  let file, line = new_line file 1 in 
  Gfx.debug "ligne 1 : %s\n%!" line;
  let joueur = String.split_on_char ' ' line in
  let joueur, x_player = int_carac joueur 1 in
  let joueur, y_player = int_carac joueur 1 in
  let player = Player.create "player" x_player y_player 10 10 red in 
  (* les directions sont pas encore prises en compte*)
  let camera = Camera.create "camera" 0 0 800 600 in 
  (*Ligne 2 labyrinthe*)
  let file, line = new_line file 2 in 
  Gfx.debug "ligne 2 : %s\n%!" line;
  let lab = String.split_on_char ' ' line in 
  let lab, x_lab = int_carac lab 2 in 
  let lab, y_lab = int_carac lab 2 in 

  (*Ligne 3*)
  let file, line = new_line file 3 in 
  Gfx.debug "ligne 3 : %s\n%!" line;

  (*Ligne 4 murs*)
  let line = "remplissage" in 
  let file = load_walls line file x_lab y_lab 0 in 
  


  (*ligne 5*)

  (*ligne 6*)
  let file, line = new_line file 6 in 
  Gfx.debug "%s\n%!" line;

  (*ligne 7 : les zones*)
  let line ="remplissage" in 
  ignore(load_zone line file x_lab y_lab);  
  
  player, camera










(*while (!line != "\n") do (*Je sais pas si ça marche, à tester quand le bordel voudra bien marcher*)
    (*ligne par ligne*)
    
    let x_mur = !ligne * taille_case + x_lab in 
    let line = ref (input_line file) in 
    let ligne_murs = String.split_on_char  ' ' !line in 
    while (not(List.is_empty ligne_murs)) do 
      (*colonne par colonne*)
      let y_mur = !colonne * taille_case + y_lab in 
      let ligne_murs, mur = next_carac ligne_murs (3+ !ligne) in 
      let murs = String.split_on_char ';' mur in 
      (*les 3 carac d'une case*)
      for i = 1 to 3 do 
        let murs, mur = next_carac murs (3+ !ligne) in 
        match mur with 
        "h" -> 
          ignore(Wall.create (Printf.sprintf "mur_haut_%n:%n" x_mur y_mur) x_mur y_mur 200 10 black) 
        |"b" -> let y_mur = y_mur + 190 in 
          ignore(Wall.create (Printf.sprintf "mur_bas_%n:%n" x_mur y_mur) x_mur y_mur 200 10 black)
        |"g" -> 
          ignore(Wall.create (Printf.sprintf "mur_gauche_%n:%n" x_mur y_mur) x_mur y_mur 10 200 black)
        |"d" -> let x_mur = x_mur + 190 in 
          ignore(Wall.create (Printf.sprintf "mur_droite_%n:%n" x_mur y_mur) x_mur y_mur 10 200 black)
        |"0" -> ()
        |_ -> Gfx.debug "Mur mal créé ligne n°%n colonne n°%n \n%!" !ligne !colonne;
          failwith (Printf.sprintf "Mur mal créé ligne n°%n colonne n°%n " !ligne !colonne ) 

      done; (*fin de la case*)
      colonne := !colonne +1
    done; (*fin de la ligne*)
    colonne := 0;
    ligne := !ligne +1
  done ; (*fin de la partie mur WOUHOUUUUUU*)
  *)






  (*while (!line != "\n") do 
    let line = ref (input_line file) in 
    let case = String.split_on_char ' ' !line in 
    let case, x_zone = int_carac case 7 in 
    let x_zone = x_zone * 100 - x_lab in
    let case, y_zone = int_carac case 7 in 
    let y_zone = y_zone * 100 - y_lab in 
    let case, type_zone = next_carac case 7 in 
    match type_zone with 
    "1" -> let case, dir_zone = next_carac case 7 in 
           let dir_zone = ref (String.split_on_char ';' dir_zone) in 
           let haut,bas,gauche,droite = ref false, ref false, ref false, ref false in 
           while (not(List.is_empty !dir_zone)) do 
            match List.hd !dir_zone with 
              "h" -> haut := true;
              |"b"-> bas := true;
              |"g"-> gauche := true;
              |"d"-> droite := true;
              |_ -> Gfx.debug "Mauvaise direction enregistrée sur la case %n:%n" x_zone y_zone;
                    failwith (Printf.sprintf "Mauvaise direction enregistrée sur la case %n:%n" x_zone y_zone)
            dir_zone := match !dir_zone with a::l -> l |_ -> [];
          done;
          ignore(Zone.create_moov (Printf.sprintf "Zone_Moov_%n:%n" x_zone y_zone) x_zone y_zone 100 100 violet !haut !bas !gauche !droite)
    |"2" -> ignore(Zone.create (Printf.sprintf "Zone_Death_%n:%n" x_zone y_zone) x_zone y_zone 100 100 2)
    |"3" -> let case, sibling = next_carac case 7 in
            ignore(Zone.create_tp_entree (Printf.sprintf "Tp_enter_%n:%n" x_zone y_zone) sibling x_zone y_zone 100 100)
    |"4" -> let case, sibling = next_carac case 7 in 
            ignore(Zone.create sibling x_zone y_zone 100 100 4)
    |"S" -> ()
    |_ -> ()

  done;
*)
