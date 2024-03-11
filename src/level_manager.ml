open Component_def
open System_def


let level_table = Hashtbl.create 8

let paths = ["resources/files/test.level";
             "resources/files/01.level";
             "resources/files/02.level";
             "resources/files/03.level";
             "resources/files/04.level";
             "resources/files/05.level"
             ]

let load_all_level _dt =
  List.iter (fun path -> Hashtbl.add level_table path (Gfx.load_file path)) paths;
  false

let wait_all_level _dt = 
  let all_loaded = Hashtbl.fold (fun _p txt acc -> Gfx.resource_ready txt && acc) level_table true in 
  not (all_loaded)


let create_file lvl = 
  let path = match lvl with
  0 -> "resources/files/test.level"
  |1 -> "resources/files/01.level"
  |2 -> "resources/files/02.level"
  |3 -> "resources/files/03.level"
  |4 -> "resources/files/04.level"
  |5 -> "resources/files/05.level"
  |_ -> Gfx.debug "Pas de niveau \n%!";
        failwith "Pas de niveau"
  in
  let level = Hashtbl.find level_table path in 
  try Gfx.get_resource level with e ->let error = Printexc.to_string e in Gfx.debug "%s \n%!" error;
  failwith "Je sais pas honnêtement"