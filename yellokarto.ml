(*** Init ***)
let moteur3D = new Moteur3D.moteur3d
let skybox = new Skybox.skybox
let traitement = new Traitement_image.traitement
let modelisation = new Modelisation.modelisation traitement

let display_state = false
let image_path_genuine = ref ""
let file_path = ref "" (* variable ou on stock le chemin de l'image*)
let acc = ref 0
let img_not_empty = ref false
let filter_state = ref false (*
                              * Si state = 0, grille = false
                             * Si state = 1, grille = true  
                              *)
let _ = GMain.init () (* initialisation du main *)
  
let remove_img file = if(Sys.file_exists file) then (Sys.remove file)
  
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
    
let up_menu = GMenu.menu_bar ~packing:vboxMenu#add ()
  
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
    
let banner = 
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
let notebook = GPack.notebook
  ~homogeneous_tabs:true
  ~show_border:true
  ~show_tabs:true
  ~border_width:4
  ~width:700
  ~height:550
  ~packing:vboxUnderToolbar#pack ()
  
let tab_text1 = GMisc.label ~text:"Présentation" ()
let tab_text2 = GMisc.label ~text:"Option de modélisation" ()
let tab_text3 = GMisc.label ~text:"Modélisation 3D" ()
  
(* ajoute les 4 onglets *)
let tab1 =
  GPack.hbox
    ~homogeneous:false
    ~spacing:4
    ~border_width:4 () 
    
let addtab1 = notebook#insert_page
  ~tab_label:tab_text1#coerce tab1#coerce
  
let tab2 =
  GPack.hbox
    ~homogeneous:false
    ~spacing:4
    ~border_width:4 ()
    
let addtab2 = notebook#insert_page
  ~tab_label:tab_text2#coerce tab2#coerce
  
  
  
let tab3 =
  GPack.vbox
    ~homogeneous:false
    ~spacing:4
    ~border_width:4 ()
    
let addtab3 = notebook#insert_page
  ~tab_label:tab_text3#coerce tab3#coerce
  
let window3D = GBin.frame
  ~label:"Rendu 3D"
  ~packing:tab3#add ()
  
let layout_3D = GPack.fixed
  ~border_width:1
  ~packing:window3D#add ()
  
let vboxTab2 =
  GPack.vbox 
    ~spacing:4
    ~border_width:4
    ~packing:tab2#add ()
    
    
let hboxcheck =
  GPack.hbox
    ~spacing:4
    ~border_width:4
    ~packing:vboxTab2#add ()
    
let hboxOption =
  GPack.hbox 
    ~spacing:4
    ~border_width:4
    ~packing:vboxTab2#add ()
    
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
    
(*** Création des boites pour afficher les images  ***)
    
    
(* fonction qui affiche l'image *)
let image = 
  GMisc.image
    ~file:"images/Kassocié.png"
    ~packing:tab2#add()
    
(*** Fonction qui charge l'image ***) 
(* fonction appelé lorsque l'on clique sur le bouton parcourir *)
    
let load_img text =
  (* met le lien du fichier dans file_path *)
  image#set_file text;
  img_not_empty := true;
  flush stdout
    
let validateColor color height button _ =
  button#set_label (height#text^" verrouillé");
  let (r,g,b) = color in
    modelisation#create_height (r,g,b,(int_of_string height#text))
      
let button_color_list = ref []    
  (* affiche la liste des couleurs trouvé *)
let rec displayColors l =  ignore (modelisation#get_list_colours (!file_path));
   let value_h = ref (((List.length l)-1)*50) in
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
            (if((r,g,b) <> (0,0,255)) then entry#set_text
               (string_of_int(!value_h)) else (entry#set_text "0");
               value_h :=  !value_h - 50;
            ignore(button_color_list := ((r,g,b), entry, button)::(!button_color_list));
             ignore(button#connect#clicked ~callback:(validateColor (r,g,b) entry button)));
            button#set_border_width 20;
          end;
          displayColors l
    | _ -> ()

let rec validate_all = function
    |((r,g,b), entry, button)::l -> validateColor (r,g,b) entry button (); validate_all l
    | _ -> ()

let rec remove_all = function
    |((r,g,b), entry, button)::l -> button#destroy (); entry#destroy (); remove_all l
    | _ -> ()

let display_colors _ = if(!img_not_empty && not display_state) then
  ((displayColors (modelisation#get_list_colours !file_path));
   img_not_empty := false)
    
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
    if dlg#run() = `OPEN then ((file_path := (str_op(dlg#filename)));
                 remove_all !button_color_list;
			     image_path_genuine := (str_op(dlg#filename));     
			     traitement#blackborder !file_path;
			     file_path := (!file_path^".bb.bmp");
			     load_img !file_path;
			     display_colors ();
			     notebook#goto_page 1);
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
      
      
let enable_triangles = ref true

let set_triangles triangles = (if(!enable_triangles) 
    then enable_triangles := false
    else enable_triangles := true);
    !enable_triangles
(*** configuration de menu en haut ***)
let fileEntries ()=
  [
    `I ("Ouvrir image",openBox);
    `S;
    `I ("Quitter",(remove_img !file_path;GMain.quit));
  ]
    
let editEntries ()=
  [
    `I ("Valider toutes les hauteurs", fun _ -> validate_all !button_color_list);
    
    `I ("Générer le terrain", fun _-> notebook#goto_page 2;
	  modelisation#create_obj_file !image_path_genuine);
  ]
    
let toolEntries ()=
  [
    `I ("Changer de mode de vue",(fun _ -> moteur3D#set_triangles
    (set_triangles ())));
    `S;
    `I ("Monter carte", fun _-> ());
    
  ]
    
let infoEntries ()=
  [
    `I ("A propos",about_button);
  ]
    
let optionList = GMenu.menu_item
  ~label:"Fichier"
  ~packing:up_menu#append
  ()
  
let optionEdit = GMenu.menu_item
  ~label:"Edition"
  ~packing:up_menu#append
  ()
  
let optionTool = GMenu.menu_item
  ~label:"Outils"
  ~packing:up_menu#append
  ()
  
let optionInfo = GMenu.menu_item
  ~label:"?"
  ~packing:up_menu#append
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
  
let button_grill =
  GPack.vbox
    ~spacing:4
    ~border_width:4
    ~homogeneous:false
    ~packing:hboxcheck#add ()
    
let update_grill =
  GPack.vbox
    ~spacing:4
    ~border_width:4
    ~homogeneous:false
    ~packing:hboxcheck#add ()
    
(* GMisc.label > affiche la précision de l'échantillonnage *)
let labelgrill =
  let label = GMisc.label
    ~text:"Pas de la grille."
    ~packing:update_grill#add ()
  in label

       
(* GRange.scale > permet de régler la précision *)
let fixgrill =
  let button = GRange.scale `HORIZONTAL
    ~adjustment:(GData.adjustment ~lower:5. ~upper:50. ())
    ~value_pos:`LEFT
    ~digits:0
    ~inverted:false
    ~packing:update_grill#add ()
  in button
       
(* Récupère l'état de filter_state pour savoir quelle fonction appliqué *)
let set_filter _ = match !filter_state with
    true -> image#set_file !file_path; filter_state := false
  | false -> traitement#createGrill !file_path (int_of_float
		(fixgrill#adjustment#value)); filter_state := true;
      image#set_file (!file_path^".grille.bmp");
      (Sys.remove (!file_path^".grille.bmp"))
	
let check_button_grill =
  let button = GButton.check_button
    ~label:"Affichage de la grille"
    ~active:false
    ~packing:button_grill#add () in
    button#connect#clicked set_filter
      
      
      
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
    ignore(button#connect#clicked (fun () -> GMain.quit ();remove_img
    !file_path; Sys.remove "map3d.obj"));
    button
      
      
let home_text =
  let text = open_in "text/presentation.txt" in
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
	
let area = GlGtk.area
  [ `RGBA; `DOUBLEBUFFER; `BUFFER_SIZE 8 ;`DEPTH_SIZE 16]
  ~width:978
  ~height:470
  ~show:true
  ~packing:layout_3D#add ()
  
let initGLenable = ref false
let stopinitGL = ref false

let mouse_released t =
moteur3D#mouse_pressed (GdkEvent.Button.button t) false
    (int_of_float (GdkEvent.Button.x t))
    (int_of_float (GdkEvent.Button.y t));
  false

let mouse_press t =
  moteur3D#mouse_pressed (GdkEvent.Button.button t) true
    (int_of_float (GdkEvent.Button.x t))
    (int_of_float (GdkEvent.Button.y t));
  false
    
let mouse_motion t =
  moteur3D#motionMouse
    (int_of_float (GdkEvent.Motion.x t))
    (int_of_float (GdkEvent.Motion.y t));
  true
    
let rec initthisgl () =
  if(notebook#current_page = 2) then
    (if not(!initGLenable) then
       (notebook#goto_page 2;
	area#make_current ();
	ignore(moteur3D#initGL);
    initGLenable := true;
	skybox#create_texture_from_image "sky/sky5.jpg";
	stopinitGL := true);
     ignore(GMain.Timeout.add ~ms:50 ~callback:(fun () -> initthisgl();false)))
      
      
let refresh_area () = 
  if (notebook#current_page = 2) then 
    begin
      area#make_current ();
      moteur3D#display3D;
      skybox#draw_skybox;
      area#swap_buffers ()
    end
      
let init_area ()=
  area#event#add [`ALL_EVENTS ; `BUTTON_MOTION ; `BUTTON_PRESS ; `BUTTON_RELEASE];
  ignore(area#event#connect#motion_notify  ~callback:mouse_motion);
  ignore(area#event#connect#button_release ~callback:mouse_released);
  ignore(area#event#connect#button_press ~callback:mouse_press);
  ignore(GMain.Timeout.add ~ms:5 ~callback:(fun () -> 
					      initthisgl();not(!stopinitGL)));
  ignore(GMain.Timeout.add ~ms:30 ~callback:(fun () -> refresh_area();true));
  ignore(area#connect#display ~callback:(fun () ->refresh_area()))

    
    
let _ =
  ignore(mainWindow#connect#destroy (remove_img !file_path;GMain.quit));  
  ignore(Glut.init Sys.argv);
  init_area ();
  mainWindow#show ();
  GMain.main ()
