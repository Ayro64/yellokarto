(*
 * yellokarto.ml - Main file for GUI management
 * Licence: LGPLv3
 * Authors: Theo Sellem, Yello Karto
 *)

(*** Init ***)
let display_state = false
let file_path = ref "images/Kassocié.png" (* variable ou on stock le chemin de l'image*)
let acc = ref 0
let img_not_empty = ref false
let filter_state = ref 0 (*
                          * Si state = 0, grille & separate colors = 0,
                          * Si state = 1, grille = 1 & separate colors = 0
                          * Si state = 2, grille = 0 & separate colors = 1
                          * Si state = 3, grille & separate colors = 1 
                          *)
let _ = GMain.init () (* initialisation du main *)


(* Fenêtre principale (non redimensionnable). *)
let mainWindow = GWindow.window
  ~width:1024
  ~height:700
  ~resizable:false
  ~title:"Yello Karto" ()

(*** Menu du haut ***)
  (* première box ajouté a la fenetre mainWindow principale *)
let vboxMenu =
  GPack.vbox
    ~spacing:4
    ~border_width:4
    ~packing:mainWindow#add ()

let menu_haut = GMenu.menu_bar ~packing:vboxMenu#add ()

let toolbar = GButton.toolbar
        ~orientation:`HORIZONTAL
        ~style:`BOTH
        ~height:80
        ~packing:vboxMenu#pack ()

let vboxUnderToolbar =
  GPack.vbox
    ~spacing:4
    ~border_width:4
    ~packing:vboxMenu#add ()

let vboxOpen =
  GPack.vbox
    ~border_width:10
    ~spacing:100
    ~homogeneous:false
    ~packing:toolbar#add ()

let separator = 
    GMisc.separator `VERTICAL
    ~packing:toolbar#add ()

let banniere = 
  GMisc.image
    ~file:"images/montagne.jpg"
    ~width:800
    ~packing:toolbar#add()

let separator2 = 
    GMisc.separator `VERTICAL
    ~packing:toolbar#add ()

let vboxInfo =
GPack.vbox
    ~border_width:10
    ~homogeneous:false
    ~packing:toolbar#add ()

(*** Ajout des onglets ***)
(* NOTEBOOK *)
let notebook =
    GPack.notebook
      ~homogeneous_tabs:true
      ~show_border:true
      ~show_tabs:true
      ~border_width:4
     ~width:700
      ~height:550
     ~packing:vboxUnderToolbar#pack ()

let text_onglet1 = GMisc.label ~text:"Présentation" ()
let text_onglet2 = GMisc.label ~text:"Option de modélisation" ()
let text_onglet3 = GMisc.label ~text:"Modélisation 3D" ()

(* ajoute les 4 onglets *)
let onglet1 =
  GPack.hbox
    ~homogeneous:false
    ~spacing:4
    ~border_width:4 () 

let addonglet1 = notebook#insert_page
  ~tab_label:text_onglet1#coerce onglet1#coerce

let onglet2 =
  GPack.hbox
    ~homogeneous:false
    ~spacing:4
    ~border_width:4 ()

let addonglet2 = notebook#insert_page
  ~tab_label:text_onglet2#coerce onglet2#coerce


let onglet3 =
  GPack.vbox
    ~homogeneous:false
    ~spacing:4
    ~border_width:4 ()

let addonglet3 = notebook#insert_page
  ~tab_label:text_onglet3#coerce onglet3#coerce

(*** Création des boites pour afficher les images  ***)


(* fonction qui affiche l'image *)
let image = 
  GMisc.image
    ~file:!file_path
    ~packing:onglet2#add()

 (*** Fonction qui charge l'image ***) 
(* fonction appelé lorsque l'on clique sur le bouton parcourir *)

let load_img text =
    file_path := text; (* met le lien du fichier dans file_path *)
  image#set_file !file_path;
  img_not_empty := true;
  flush stdout

(*** action lier a menu en haut ***)

exception IsNone

let str_op = function
  | Some x -> x
  | _ -> raise IsNone

let img_filter = GFile.filter
  ~name:"Img File"
  ~patterns:["*.bmp"; "*.jpg"; "*.jpeg "; "*.png"]()

let openBox _ =
  let dlg = GWindow.file_chooser_dialog
    ~action:`OPEN
    ~parent:mainWindow
    ~position:`CENTER_ON_PARENT
    ~title: "Chargement d'une image"
    ~destroy_with_parent:true () in
  dlg#set_filter img_filter;
  dlg#add_button_stock `CANCEL `CANCEL;
  dlg#add_select_button_stock `OPEN `OPEN;
  if dlg#run() = `OPEN then (load_img(str_op(dlg#filename)));
  dlg#misc#hide ()

(* About button with credits, license, website *)

let about_button _ =
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
    ~parent:mainWindow
    ~destroy_with_parent:true () in
dialog#set_name "Yello Karto";
ignore(dialog#run ());
dialog#misc#hide ()


(*** configuration de menu en haut ***)
let fileEntries ()=
  [
    `I ("Ouvrir image",openBox);
       `S;
    `I ("Quitter",GMain.Main.quit);
  ]

let editEntries ()=
  [
    `I ("Traitement de l'image", fun _-> ());
    `I ("Generer le terrain", fun _-> ());
  ]

let toolEntries ()=
  [
    `I ("Prendre une capture d'écran", fun _-> ());
    `I ("Prendre une capture d'écran sous", fun _-> ());
    `S;

    `I ("Ajouter waypoint", fun _-> ());

    `I ("Démarer le chemin", fun _-> ());

    `I ("Mise à zéro du chemin", fun _-> ());


    `S;

    `I ("Monter carte", fun _-> ());

    `I ("Déscendre carte", fun _-> ());

    `S;

    `I ("Démarrer la capture de video", fun _-> ());

    `I ("Stopper la capture de video", fun _-> ());

  ]

let infoEntries ()=
  [
    `I ("A propos",about_button);
  ]

let optionList = GMenu.menu_item
                     ~label:"Fichier"
                     ~packing:menu_haut#append
                     ()

let optionEdit = GMenu.menu_item
                     ~label:"Edition"
                     ~packing:menu_haut#append
                     ()

let optionTool = GMenu.menu_item
                     ~label:"Outils"
                     ~packing:menu_haut#append
                     ()

let optionInfo = GMenu.menu_item
                     ~label:"?"
                     ~packing:menu_haut#append
                     ()

let menu_option = GMenu.menu ~packing:optionList#set_submenu ()
let menu_edit = GMenu.menu ~packing:optionEdit#set_submenu ()
let menu_tool = GMenu.menu ~packing:optionTool#set_submenu ()
let menu_info = GMenu.menu ~packing:optionInfo#set_submenu ()

let toolbox = GToolbox.build_menu
      menu_option
      ~entries:(fileEntries ())


let toolbox = GToolbox.build_menu
      menu_edit
      ~entries:(editEntries ())

let toolbox = GToolbox.build_menu
      menu_tool
      ~entries:(toolEntries ())

let toolbox = GToolbox.build_menu
      menu_info
      ~entries:(infoEntries ())



(* Option de Traitement *)

let vboxOnglet2 =
    GPack.vbox 
  ~spacing:4
  ~border_width:4
  ~packing:onglet2#add ()


let hboxcheck =
  GPack.hbox
  ~spacing:4
  ~border_width:4
  ~packing:vboxOnglet2#add ()

let hboxOption =
  GPack.hbox 
  ~spacing:4
  ~border_width:4
  ~packing:vboxOnglet2#add ()

let vboxOption1 =
  GPack.vbox
    ~spacing:4
    ~border_width:4
    ~homogeneous:false
    ~packing:hboxOption#add ()

let vboxOption2 =
  GPack.vbox
    ~spacing:4
    ~border_width:4
    ~homogeneous:false
    ~packing:hboxOption#add ()

 (* Récupère l'état de filter_state pour savoir quelle fonction appliqué *)
let set_filter () = match !filter_state with
    | 0 -> image#set_file !file_path
    | 1 -> Traitement_image.createGrill !file_path () 25;
           image#set_file (!file_path^".grille.bmp");
           (Sys.remove (!file_path^".grille.bmp"))
    | 2 -> Traitement_image.blackborder !file_path ();
           image#set_file (!file_path^".bb.bmp");
           (Sys.remove (!file_path^".bb.bmp"))
    | 3 -> Traitement_image.separateGrill !file_path () 25;
           image#set_file (!file_path^".bbgrille.bmp");
           (Sys.remove (!file_path^".bbgrille.bmp"))
    | _ -> ()


    (* Récupère l'état courant du filtre appliqué à l'image pour savoir quelle
     * fixer le nouvelle état et par la suite rappeler set_filter  *)

let get_filter_separate () = match !filter_state with 
     0 -> filter_state := 2
    |1 -> filter_state := 3
    |2 -> filter_state := 0
    |3 -> filter_state := 1
    |_ -> ()

let get_filter_grill () = match !filter_state with 
     0 -> filter_state := 1
    |1 -> filter_state := 0
    |2 -> filter_state := 3
    |3 -> filter_state := 2
    |_ -> ()


    (* affiche la liste des couleurs trouvé *)
let rec displayColors l =  ignore (Modelisation.get_list_colours (!file_path));
        match l with
         x::l -> let (r,g,b) = x in
             let box = GPack.hbox
                ~packing:(if(!acc mod 2 = 0) then vboxOption1#add
                                            else vboxOption2#add) ()
         in ignore (box); (acc := !acc + 1);
               let entry = GEdit.entry
                        ~text:""
                        ~width:60
                        ~height:30
                        ~packing:box#add ()
                and  button = GButton.button
                        ~label:"Valider"
                        ~packing:box#add ()
                in
                     let color = Printf.sprintf "#%02x%02x%02x" r g b
                in
                begin
                  entry#misc#modify_base [`NORMAL, `NAME color];
                  button#set_border_width 20;
                end;
                displayColors l
        | _ -> ()

let display_colors _ = if(!img_not_empty && not display_state) then
        ((displayColors (Modelisation.get_list_colours !file_path));
        img_not_empty := false)

let check_button_separate_colors =
         let button = GButton.check_button
       ~label:"Séparation des couleurs"
       ~active:false
       ~packing:hboxcheck#add () in
         button#connect#clicked (fun () -> get_filter_separate (); set_filter (); display_colors ())

let check_button_grille =
            let button = GButton.check_button
       ~label:"Affichage de la grille"
       ~active:false
      ~packing:hboxcheck#add () in
            button#connect#clicked (fun () -> get_filter_grill (); set_filter (); display_colors ())

let display3Dbutton =
  let button = GButton.button
    ~label:"Affichage 3D"
    ~packing:onglet3#pack () in
  button

(*** Button Open, About & Quit, Afficher un texte à l'écran ***)


let file_button =
  let button = GButton.button
    ~label:"Parcourir"
    ~stock:`OPEN
    ~packing:vboxOpen#add () in 
  (* On met une petite image kikoo pour l'ouverture du bouton *)
    ignore (GMisc.image ~stock:`OPEN ~packing:button#set_image ());
  (* permet d'ouvrir une fenetre Parcourir  *)
    ignore (button#connect#clicked openBox);
  button 


let about =
	let button = GButton.button 
	~stock:`ABOUT 
	~packing:vboxInfo#add() in
  ignore (GMisc.image ~stock:`ABOUT ~packing:button#set_image ());
  ignore (button#connect#clicked about_button);
  button

let quit = 
      let button = GButton.button
          ~stock:`QUIT
          ~packing:vboxInfo#add () in
       ignore( button#connect#clicked GMain.quit);
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
                        ~packing:onglet1#add ()
(*
let time =
  let start = Unix.gettimeofday () in
  fun () -> Unix.gettimeofday () -. start


let initGL _ =
	GlMat.mode `projection;
	GlMat.load_identity ();
	GluMat.perspective ~fovy:45.0 ~aspect:(800.0/.600.) ~z:(0.1, 1000.);
	GluMat.look_at (3., 4., 2.) (0., 0., 0.) ( 0., 0., 1.);
	GlMat.mode `modelview;
	GlMat.load_identity ();
	GlClear.clear [`depth ; `color];
	Gl.enable `depth_test
   

let display3D _ =
GlClear.clear [`depth ; `color];
	GlMat.rotate ~angle:(-100. *. time ()) ~x:1. ~y:1. ~z:0. ();
	
	GlDraw.begins `quads;
	
	GlDraw.color (1., 0., 0.);
	GlDraw.vertex3 (1., 1., 1.);
	GlDraw.vertex3 (1., 1., -1.);
	GlDraw.vertex3 (-1., 1., -1.);
	GlDraw.vertex3 (-1., 1., 1.);
	
	GlDraw.color (0., 1., 0.);
	GlDraw.vertex3 (1., -1., 1.);
	GlDraw.vertex3 (1., -1., -1.);
	GlDraw.vertex3 (1., 1., -1.);
	GlDraw.vertex3 (1., 1., 1.);
	
	GlDraw.color (0., 0., 1.);
	GlDraw.vertex3 (-1., -1., 1.);
	GlDraw.vertex3 (-1., -1., -1.);
	GlDraw.vertex3 (1., -1., -1.);
	GlDraw.vertex3 (1., -1., 1.);
	
	GlDraw.color (0.8, 0.1, 0.89);
	GlDraw.vertex3 (-1., 1., -1.);
	GlDraw.vertex3 (1., 1., -1.);
	GlDraw.vertex3 (1., -1., -1.);
	GlDraw.vertex3 (-1., -1., -1.);
	
	GlDraw.color (0.7, 0.5, 0.12);
	GlDraw.vertex3 (-1., 1., 1.);
	GlDraw.vertex3 (-1., 1., -1.);
	GlDraw.vertex3 (-1., -1., -1.);
	GlDraw.vertex3 (-1., -1., 1.);
	
	GlDraw.color (0.05, 0.60, 1.);
	GlDraw.vertex3 (-1., 1., 1.);
	GlDraw.vertex3 (1., 1., 1.);
	GlDraw.vertex3 (1., -1., 1.);
	GlDraw.vertex3 (-1., -1., 1.);	
	GlDraw.ends ();	
	Gl.flush () *)

let area = 
  let b =
    GlGtk.area [`RGBA;`DEPTH_SIZE 1;`DOUBLEBUFFER]
      ~width:800 ~height:600 ~packing:onglet3#add () in
    ignore(b#connect#realize ~callback:Moteur3D.initGL);	
    ignore(b#connect#display (fun () -> Moteur3D.display3D (); b#swap_buffers ()));
    b
     
   
   let _ =
 ignore( mainWindow#connect#destroy GMain.quit);
  mainWindow#show ();
  GMain.main ()

