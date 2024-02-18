open Component_def
open System_def

let taille_case = 200


let red = Texture.color (Gfx.color 255 0 0 255)
let black = Texture.color (Gfx.color 0 0 0 255)



let next_carac line i =
  match line with
  a::l -> l,a
  |_ -> Gfx.debug "Pb ligne n°%n du fichier de level \n%!" i;
  failwith (Printf.sprintf "Pb ligne n° %n du fichier de level" i)

  let int_carac line i =
    let l,a = next_carac line i in 
    l, int_of_string a  

let load_lvl lvl =
  Gfx.debug "Debut du loading\n%!";
  let path = match lvl with
  0 -> "resources/files/test.level"
  |1 -> "resources/files/01.level"
  |2 -> "resources/files/02.level"
  |_ -> Gfx.debug "Pas de niveau \n%!";
        failwith "Pas de niveau"
  in
  Gfx.debug "C'est pas le path\n%!";
  try 
    let file = open_in path in 
  Gfx.debug "C'est pas le file\n%!";
  (*Ligne 1 joueur*)
  let line = ref (input_line file) in 
  Gfx.debug "ligne 1 : %s\n%!" !line;

  let joueur = String.split_on_char ' ' !line in
  let joueur, x_player = int_carac joueur 1 in
  let joueur, y_player = int_carac joueur 1 in
  let player = Player.create "player" x_player y_player 10 10 red in 
  (* les directions sont pas encore prises en compte*)
  let camera = Camera.create "camera" 0 0 800 600 in 
  (*Ligne 2 labyrinthe*)
  let line = ref (input_line file) in 
  Gfx.debug "ligne 2 : %s\n%!" !line;
  let lab = String.split_on_char ' ' !line in 
  let lab, x_lab = int_carac lab 2 in 
  let lab, y_lab = int_carac lab 2 in 

  (*Ligne 3*)
  let line = ref (input_line file) in 
  Gfx.debug "ligne 3 : %s\n%!" !line;

  (*Ligne 4 murs*)

  let ligne = ref 0 in 
  let colonne = ref 0 in 
  let line = ref "remplissage" in 
  while (!line != "\n") do (*Je sais pas si ça marche, à tester quand le bordel voudra bien marcher*)
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



(*ligne 5*)

(*ligne 6*)
let line = ref (input_line file) in 
Gfx.debug "%s\n%!" !line;

(*ligne 7 : les zones*)






  close_in file;
  player, camera
with e -> Gfx.debug "Le fichier s'ouvre pas\n%!";
raise e