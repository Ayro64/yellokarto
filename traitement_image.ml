let loadImage file =
  Sdlloader.load_image file
    
let saveImage surface file =
  Sdlvideo.save_BMP surface file
    
let sumRGB (r,g,b) = r + g + b

(* Dimensions d'une image *)
let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, 
   (Sdlvideo.surface_info img).Sdlvideo.h)
    
(* transformation en gris *)
let level (r,g,b) = float_of_int(r) *. 0.3 +.  float_of_int(g) *. 0.59  +.
  float_of_int(b) *. 0.11
  
let rec is_in_list elt list = match list with
  |[] -> false
  |e::l when e=elt -> true
  |e::l -> is_in_list elt l
     
let add_if_new elt list = match (elt, list) with
  |(x,l) when (not(is_in_list x l)) ->  x::l
  |(x,l) -> l

  
(* séparation des couleurs avec ligne noire *)
  
let everCreate = ref false
  
  
let image2bb img =
  let (w,h) = get_dims img in
    for i = 0 to h-2 do
      for j = 0 to w-2 do
        if((sumRGB (Sdlvideo.get_pixel_color img j i)) <> 
	     (sumRGB (Sdlvideo.get_pixel_color img (j+1) (i)))) then
          (Sdlvideo.put_pixel_color img j i (0,0,0));
        if((sumRGB (Sdlvideo.get_pixel_color img j i)) <> 
	     (sumRGB (Sdlvideo.get_pixel_color img (j) (i+1)))) then
          (Sdlvideo.put_pixel_color img j i (0,0,0))
      done
    done
      
let blackborder filepath  =
  begin
    let img = loadImage filepath in
      (*on applique la transformation*)
      image2bb img;
      saveImage img (filepath^".bb.bmp");
  end
    let points_list = ref []
    let triangles_templist = ref []

let make_trianglelist img n =
  let (w,h) = get_dims img in
    for i=0 to (h-1) do
      for j=0 to (w-1) do
	if (j mod n = 0 && i mod n = 0) then
	  if((i+n)<h && (j+n)<w) then 
		triangles_templist := (j,i)::(!triangles_templist);
		triangles_templist := (j+n,i)::(!triangles_templist);
		triangles_templist := (j,i+n)::(!triangles_templist);
	  if((i-n)>=0 && (i-n)>=0) then
		triangles_templist := (j,i)::(!triangles_templist);
		triangles_templist := (j-n,i)::(!triangles_templist);
		triangles_templist := (j,i-n)::(!triangles_templist);
      done
    done




let clean_point_list () = if ((List.length !points_list) > 0) then (points_list := []; triangles_templist := [])

let image2grill img n = clean_point_list (); make_trianglelist img n;
  let (w,h) = get_dims img in
    let grill image j i =
      begin
	      if(Sdlvideo.get_pixel_color image j i = (0,0,0)) then
		points_list := add_if_new (j,i) (!points_list);
	      Sdlvideo.put_pixel_color image j i (0,0,0);
      end in
        for i=0 to (h-1) do
          for j=0 to (w-1) do
            if (j mod n = i mod n) then grill img j i;
            if(i mod n = 0) then  grill img j i;
           if(j mod n = 0) then grill img j i;
	   done;
        done
     
let createGrill filepath n =
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
    
(* Initialisation de SDL *)
let _ = sdl_init ();
