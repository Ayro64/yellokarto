let l1 =
[
    ("feuille","leaf");
    ("arbre","tree");
    ("noeud","node");
    ("sommet","vertice");
    ("arc","edge");
  ]

let assoc_option x l=
 try Some(List.assoc x l) with Not_found -> None

let _ =
    let a = assoc_option "feuille" l1 in
    match a with
    None -> print_endline "rien trouve"
  |  Some a -> print_endline a

