let loadImage file =
      Sdlloader.load_image file

let saveImage surface file =
      Sdlvideo.save_BMP surface file

let sumRGB (r,g,b) = r + g + b

let oc = open_out "test"
  
let rec print_list list () = match list with
  |[] -> close_out oc
  |(a,b)::l ->
     begin 
       output_string oc (string_of_int a ^ "-" ^ string_of_int b ^ "\n");
       print_list l ()
     end

(* attendre une touche ... *)
let rec wait_key () =
  let e = Sdlevent.wait_event () in
    match e with
    Sdlevent.KEYDOWN _ -> ()
      | _ -> wait_key ()

(*
  show img dst
  affiche la surface img sur la surface de destination dst (normalement l'écran)
*)
let show img dst =
  let d = Sdlvideo.display_format img in
    Sdlvideo.blit_surface d dst ();
    Sdlvideo.flip dst

(* Dimensions d'une image *)
let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)

(* transformation en gris *)
let level (r,g,b) = float_of_int(r) *. 0.3 +.  float_of_int(g) *. 0.59  +.
                    float_of_int(b) *. 0.11

let color2grey (r,g,b) = 
    (int_of_float(level (r,g,b)),int_of_float(level(r,g,b)),
    int_of_float(level (r,g,b)))

let image2grey img =
  let (w, h) = get_dims img in
    for i = 0 to h - 1 do
      for j = 0 to w - 1 do 
    Sdlvideo.put_pixel_color img j i 
    (color2grey (Sdlvideo.get_pixel_color img j i))
        done
     done

let greyscale filepath () =
    begin
       (* Chargement d'une image *)
    let img = loadImage filepath in
    (*on applique la transformation*)
         image2grey img;
         saveImage img (filepath^".grey.bmp");
    end 


(* séparation des couleurs avec ligne noire *)   

let image2bb img =
     let (w,h) = get_dims img in
       for i = 0 to h - 2 do
         for j = 0 to w - 2 do 
               if((sumRGB (Sdlvideo.get_pixel_color img j i)) <> (sumRGB
                (Sdlvideo.get_pixel_color img (j+1) (i)))) then
                (Sdlvideo.put_pixel_color img j i (0,0,0));
               if((sumRGB (Sdlvideo.get_pixel_color img j i)) <> (sumRGB
								    (Sdlvideo.get_pixel_color img j (i + 1)))) then
                (Sdlvideo.put_pixel_color img j i (0,0,0))
            done
         done

let blackborder filepath () =
    begin
  let img = loadImage filepath in
    (*on applique la transformation*)
         image2bb img;
         saveImage img (filepath^".bb.bmp");
    end  



(* séparation des couleurs sur ton gris *)
(* let greybb filepath () =
     begin 
         let img = loadImage filepath in
         image2grey img;
         image2bb img;        
         saveImage img (filepath^".greybb.bmp");
     end *)


(* Creation de la grille *)

let image2grill img n =
  let liste = ref [] 
  and (w,h) = get_dims img 
  and other = ref 0 in
    other := n;
    while (!other < w) do
      for i = 0 to (h - 1) do
	if (Sdlvideo.get_pixel_color img !other i = (0,0,0))
	then
	  liste := (!other, i) :: !liste;
	Sdlvideo.put_pixel_color img !other i (0,0,0)
      done;
      other := !other + n;
    done;
    other := n;
    while (!other < h) do
      for j = 0 to (w - 1) do
	if ((j mod n <> 0) && Sdlvideo.get_pixel_color img j !other = (0,0,0))
	then
	  liste := (j, !other) :: !liste;
	Sdlvideo.put_pixel_color img j !other (0,0,0)
      done;
      other := !other + n;
    done;
    print_list !liste
      (* Pour le passage des diagonales, ajouter un truc comme la ligne 120, qui exclut les i mod n et les j mod n *)
	  
	  
let createGrill filepath () n=
     begin 
         let img = loadImage filepath in
         image2grill img n;        
         saveImage img (filepath^".grille.bmp");
     end 

(* init de SDL *)
let sdl_init () =
  begin
    Sdl.init [`EVERYTHING];
    Sdlevent.enable_events Sdlevent.all_events_mask;
  end

(* main *)
let main () =
  begin
    (* Nous voulons 1 argument *)
    if Array.length (Sys.argv) < 2 then
      failwith "Il manque le nom du fichier!";
    (* Initialisation de SDL *)
    sdl_init ();
    (* Chargement d'une image *)
    let img = Sdlloader.load_image Sys.argv.(1) in
    (* On récupère les dimensions *)
    let (w,h) = get_dims img in
    (* On crée la surface d'affichage en doublebuffering *)
    let display = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
      (* on affiche l'image *)
      show img display;
      (* on attend une touche *)
      wait_key ();
      (* nouvelle surface *)
      let surf = Sdlvideo.create_RGB_surface_format img [] w h in
	(* on passe l'image en niveaux de gris*)
	image2bb img;
	(* affichage *)
	show img display;
	(* on attend une touche *)
	wait_key();
	image2grill img 10;
	show img display;
	wait_key();
	(* on quitte *)
	exit 0
  end

(* Initialisation de SDL *)
let _ = main ()
