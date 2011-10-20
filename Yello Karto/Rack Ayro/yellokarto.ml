(* yellokarto.ml - Main file for GUI management
 * Licence: LGPLv3
 * Authors: Theo Sellem, Yello Karto
 *)

let file_path = ref "images/Kassocié.png" (* variable ou on stock le chemin de l'image*)

let _ = GMain.init () (* initialisation du main *)

(* Fenêtre principale (non redimensionnable). *)
let window1 = GWindow.window
  ~width:1024
  ~height:700
  ~resizable:false
  ~title:"Yello Karto" ()

  (* première box ajouté a la fenetre window1 principale *)
let vbox1 =
  GPack.vbox
    ~spacing:4
    ~border_width:4
    ~packing:window1#add ()


let toolbar = GButton.toolbar
        ~orientation:`HORIZONTAL
        ~style:`BOTH
        ~height:80
        ~packing:vbox1#pack ()


let vboxOpen =
  GPack.vbox
    ~border_width:10
    ~spacing:100
    ~homogeneous:false
    ~packing:toolbar#add ()

let separator = 
    GMisc.separator `VERTICAL
    ~packing:toolbar#add ()

let vboxempty =
    GPack.vbox
    ~width:800
    ~packing:toolbar#add ()

let separator2 = 
    GMisc.separator `VERTICAL
    ~packing:toolbar#add ()

let vboxInfo =
GPack.vbox
    ~border_width:10
    ~homogeneous:false
    ~packing:toolbar#add ()

(* NOTEBOOK *)
let notebook1 =
    GPack.notebook
      ~homogeneous_tabs:true
      ~show_border:true
      ~show_tabs:true
      ~border_width:4
(*      ~width:700
      ~height:550
*)      ~packing:vbox1#pack ()

let label11 = GMisc.label ~text:"Présentation" ()
let label12 = GMisc.label ~text:"Traitement" ()
let label13 = GMisc.label ~text:"Option de modélisation" ()
let label14 = GMisc.label ~text:"Modélisation 3D" ()

(* ajoute les 4 onglets *)
let tab1 =
  GPack.hbox
    ~homogeneous:false
    ~spacing:4
    ~border_width:4 () 

let addonglet1 = notebook1#insert_page
  ~tab_label:label11#coerce tab1#coerce

let tab2 =
  GPack.hbox
    ~homogeneous:false
    ~spacing:4
    ~border_width:4 () 

let addonglet2 = notebook1#insert_page
  ~tab_label:label12#coerce tab2#coerce

let tab3 =
  GPack.hbox
    ~homogeneous:false
    ~spacing:4
    ~border_width:4 ()

let addonglet3 = notebook1#insert_page
  ~tab_label:label13#coerce tab3#coerce


let tab4 =
  GPack.vbox
    ~homogeneous:false
    ~spacing:4
    ~border_width:4 ()

let addonglet4 = notebook1#insert_page
  ~tab_label:label14#coerce tab4#coerce


(* Print the file chosen in the console *)
let may_print button () =
  Gaux.may print_endline button#filename

(* mettre une frame au lieu d'affiché directement l'image
    let frame = GBin.frame
    ~packing:tab2#add ()
*)

(* fonction qui affiche l'image *)
let img1 =
  GMisc.image
    ~file:!file_path
    ~packing:tab2#add () (* ou frame#add() si on utilise frame *)

let img2 = 
  GMisc.image
    ~file:!file_path
    ~packing:tab3#add()

let img3 = 
  GMisc.image
    ~file:"images/zone3D.bmp"
    ~packing:tab4#add()

  
(* fonction appelé lorsque l'on clique sur le bouton parcourir *)
let file_ok_sel filew () =
  file_path := filew#filename; (* met le lien du fichier dans file_path *)
  img1#set_file !file_path; (* appelle la fonction set file dans img1 l'image dans img1 *)
  img2#set_file !file_path;
  flush stdout
  
 (* création du boutton parcourir *) 

let imgFilter = GFile.filter
        ~name:"Fichier image"
        ~patterns:["*.bmp";"*.jpg";"*.jpeg";"*.png"]()
  
let file_button =
  let file = GWindow.file_selection
    ~title:"Selection de la map: "
    ~border_width:10
    ~parent:window1
    ~destroy_with_parent:true () in
  (* on créer le bouton*)
  let button = GButton.button
    ~label:"Parcourir"
    ~stock:`OPEN
    ~packing:vboxOpen#add () in 
  (* On met une petite image kikoo pour l'ouverture du bouton *)
    ignore (GMisc.image ~stock:`OPEN ~packing:button#set_image ());
  (* permet d'ouvrir une fenetre Parcourir  *)
   ignore (button#connect#clicked (fun () -> ignore (file#run ());  file#misc#hide ()));
    (* declenchement de l'evenement qui rapelle la fonction pour afficher
     * l'image lorsqu'on selectionne un fichier *)
  ignore (file#ok_button#connect#clicked (file_ok_sel file)); 
  button 

(* NOTEBOOK1 *)
let buttonVbox1 =
  GPack.vbox
    ~spacing:4
    ~border_width:4
    ~homogeneous:false
    ~packing:tab2#pack ()

let mapButton =
  let button = GButton.button
    ~label:"Carte"
    ~packing:buttonVbox1#add () in
(*ignore (GMisc.image ~stock:`APPLY ~packing:button#set_image ());*)
    ignore (button#connect#clicked
    (fun () -> try (img1#set_file !file_path) with _ -> ()));
  button


let greyButton =
  let button = GButton.button
    ~label:"Niveaux de gris"
    ~packing:buttonVbox1#add () in
 ignore (button#connect#clicked
  (fun () -> try ((Traitement_image.greyscale !file_path ());
  (img1#set_file ((!file_path)^".grey.bmp")); (Sys.remove (!file_path^".grey.bmp"))) with _-> ()));
     button

let blackBorderButton =
  let button = GButton.button
    ~label:"Séparation \n des couleurs"
    ~packing:buttonVbox1#add () in
  ignore (button#connect#clicked
  (fun () -> try ((Traitement_image.blackborder !file_path ());
  (img1#set_file ((!file_path)^".bb.bmp")); (Sys.remove (!file_path^".bb.bmp"))) with _-> ()));
     button



let greyBB =
  let button = GButton.button
    ~label:"Séparation \n ton gris"
    ~packing:buttonVbox1#add () in
  ignore (button#connect#clicked
  (fun () -> try ((Traitement_image.greybb !file_path ());
  (img1#set_file ((!file_path)^".greybb.bmp")); (Sys.remove (!file_path^".greybb.bmp"))) with _-> ()));
     button

let grilleButton =
  let button = GButton.button
    ~label:"Afficher \nla grille"
    ~packing:buttonVbox1#add () in
  ignore (button#connect#clicked
(* le '25' correspond au pas, il faudra le modifier *)
  (fun () -> try ((Traitement_image.createGrill !file_path () 25);
  (img1#set_file ((!file_path)^".grille.bmp")); (Sys.remove (!file_path^".grille.bmp"))) with _-> ()));
     button

(* END OF NOTEBOOK1 *)

(* NOTEBOOK2 *)
let buttonVbox2 =
  GPack.vbox
    ~spacing:4
    ~border_width:4
    ~homogeneous:false
    ~packing:tab3#pack ()

let secondaryBox =
  GPack.vbox
    ~border_width:4
    ~homogeneous:false
    ~packing:buttonVbox2#add ()

let label1 =
  GMisc.label
    ~text:"Hauteur:"
    ~packing:secondaryBox#pack ()

let adjustement =
  GData.adjustment
    ~value:0.
    ~page_size:0.
    ~step_incr:1. ()

let heightSpin1 =
  GEdit.spin_button
    ~adjustment:adjustement
    ~width:105
    ~digits:0
    ~numeric:true
    ~packing:secondaryBox#pack ()


let label1 =
  GMisc.label
    ~text:"Pas"
    ~packing:secondaryBox#pack ()

let adjustement2 =
  GData.adjustment
    ~value:1.
    ~lower:1.
    ~page_size:0.
    ~step_incr:1. ()

let heightSpin2 =
  GEdit.spin_button
    ~adjustment:adjustement2
    ~width:105
    ~digits:0
    ~numeric:true
    ~packing:secondaryBox#pack ()

let height= ref 0
let step = ref 20

let color1Button =
  let button = GButton.button
    ~label:"  Zone\nsuivante"
    ~packing:buttonVbox2#add () in
  ignore (button#connect#clicked
	    (fun () -> height:= (heightSpin1#value_as_int) ));
  ignore (button#connect#clicked
	    (fun () -> (heightSpin1#set_value 0.)));
  button

let applyButton =
  let button = GButton.button
    ~label:"Enregistrer\n les valeurs"
    ~packing:buttonVbox2#add () in
  ignore (button#connect#clicked
	    (fun () -> step := (heightSpin2#value_as_int)));
  ignore (button#connect#clicked
	    (fun () -> heightSpin2#set_value 0.));
 	     button

(* END OF NOTEBOOK2 *)

let display3Dbutton =
  let button = GButton.button
    ~label:"Affichage 3D"
    ~packing:tab4#pack () in
  button

(* About button with credits, license, website *)
let about_button =
  let dialog = GWindow.about_dialog
    ~authors:
    ["Kassociés Team: \n
Theo \"DarK_PHeONiX\" Sellem\n<theo.sellem@epita.fr>\n
Adrien \"Momo\" Moliere\n<adrien.moliere@epita.fr>\n
Etienne \"Ayro\" Marié\n<etienne.marie@epita.fr>\n
Matthieu \"The Mule\" Guyot de camy\n<matthieu.guyotdecamy@epita.fr>"]
    ~copyright:"Yello Karto © 2010-2011 By Kassociés"
    ~license:"LGPLv3"
    ~version:"v0.1" 
    ~website:"http://www.yellokarto.com"
    ~position:`CENTER_ON_PARENT
    ~parent:window1
    ~destroy_with_parent:true () in
  let button = GButton.button ~stock:`ABOUT ~packing:vboxInfo#add
    () in
  ignore (GMisc.image ~stock:`ABOUT ~packing:button#set_image ());
  ignore (button#connect#clicked
            (fun () -> ignore (dialog#run ());
              dialog#misc#hide ()));
  button

let quit = 
      let button = GButton.button
          ~stock:`QUIT
          ~packing:vboxInfo#add () in
       ignore( button#connect#clicked ~callback:GMain.quit);
                 button


let textAccueil =
        let text = open_in "presentation.txt" in
        let lenght = in_channel_length text in
        let str = String.create lenght in
        really_input text str 0 lenght;
        close_in text;
        let buffer = GText.buffer () in
                buffer#set_text (str);
                GMisc.label
                        ~text:str
                        ~width:900
                        ~line_wrap:true
                        ~packing:tab1#add ()


let _ =
 ignore( window1#connect#destroy ~callback:GMain.quit);
  window1#show ();
  GMain.main ()

