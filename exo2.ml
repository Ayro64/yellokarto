let (add_dico, find_dico) = 
     let dico = Hashtbl.create 0 in
     (
         (fun str v -> Hashtbl.add dico str v),
         (fun str -> Hashtbl.find dico str)
     )
 
let base =
  [
    ("feuille","leaf");
    ("arbre","tree");
    ("noeud","node");
    ("sommet","vertice");
    ("arc","edge");
  ]
 
let find_print w =
  try
    Printf.printf "%s: %s\n" w (find_dico w)
  with
    | Not_found -> Printf.printf "%s: pas de traduction.\n" w
 
let main () =
  begin
    List.iter (fun (k,v) -> add_dico k v) base;
    List.iter find_print
      ["feuille";"arc";"arête";"noeud";"nœud";"arbre"];
    exit 0
  end
 
let _ = main ()
