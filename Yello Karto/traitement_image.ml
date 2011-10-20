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

(* Dimensions d'une image *)
let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)

(* transformation en gris *)
let level (r,g,b) = float_of_int(r) *. 0.3 +.  float_of_int(g) *. 0.59  +.
                    float_of_int(b) *. 0.11


(* séparation des couleurs avec ligne noire *)   

let image2bb img =
     let (w,h) = get_dims img in
         for i = 0 to h-2 do
            for j = 0 to w-2 do 
               if((sumRGB (Sdlvideo.get_pixel_color img j i)) <> (sumRGB
                (Sdlvideo.get_pixel_color img (j+1) (i)))) then
                (Sdlvideo.put_pixel_color img j i (0,0,0));
               if((sumRGB (Sdlvideo.get_pixel_color img j i)) <> (sumRGB
                (Sdlvideo.get_pixel_color img (j) (i+1)))) then
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
(*
let image2grill img n =
  let (w,h) = get_dims img in
    for i=0 to (h-1) do
      for j=0 to (w-1) do
          if((i mod n = 0) || (j mod n = 0) 
          || (n-(j mod n) = i mod n) || (i mod n = j mod n)) then
	  Sdlvideo.put_pixel_color img j i (0,0,0);
      done
    done
  *) 
let createGrill filepath () n=
     begin 
         let img = loadImage filepath in
         image2grill img n ();        
         saveImage img (filepath^".grille.bmp");
     end 
          
(* séparation des couleurs + quadrillage *)
let  separateGrill filepath () n =
    begin
        let img = loadImage filepath in
        image2bb img;
        image2grill img n ();
        saveImage img (filepath^".bbgrille.bmp");
    end

(* init de SDL *)
let sdl_init () =
  begin
    Sdl.init [`EVERYTHING];
    Sdlevent.enable_events Sdlevent.all_events_mask;
  end

(* Initialisation de SDL *)
let _ = sdl_init ();

