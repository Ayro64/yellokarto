class traitement =
    object(this)
       
method private loadImage file =
  Sdlloader.load_image file
    
method private saveImage surface file =
  Sdlvideo.save_BMP surface file
    
method private sumRGB (r,g,b) = r + g + b
(* Dimensions d'une image *)
method private get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, 
   (Sdlvideo.surface_info img).Sdlvideo.h)
    
  
method private is_in_list elt list = match list with
  |[] -> false
  |e::l when e=elt -> true
  |e::l -> this#is_in_list elt l
     
method private add_if_new elt list = match (elt, list) with
  |(x,l) when (not(this#is_in_list x l)) ->  x::l
  |(x,l) -> l

  
(* séparation des couleurs avec ligne noire *)

val mutable everCreate = false
  
  
method private image2bb img =
  let (w,h) = this#get_dims img in
    for i = 0 to h-2 do
      for j = 0 to w-2 do
        if((this#sumRGB (Sdlvideo.get_pixel_color img j i)) <> 
	     (this#sumRGB (Sdlvideo.get_pixel_color img (j+1) (i)))) then
          (Sdlvideo.put_pixel_color img j i (0,0,0));
        if((this#sumRGB (Sdlvideo.get_pixel_color img j i)) <> 
	     (this#sumRGB (Sdlvideo.get_pixel_color img (j) (i+1)))) then
          (Sdlvideo.put_pixel_color img j i (0,0,0))
      done
    done
      
method blackborder filepath  =
  begin
    let img = this#loadImage filepath in
      (*on applique la transformation*)
      this#image2bb img;
      this#saveImage img (filepath^".bb.bmp");
  end

val mutable points_list = []
val mutable triangles_templist = []

method get_points_list = points_list
method get_triangles_templist = triangles_templist


method private make_trianglelist img n =
  let (w,h) = this#get_dims img in
    for i=0 to (h-1) do
      for j=0 to (w-1) do
	if (j mod n = 0 && i mod n = 0) then
	begin
	  if((i+n)<h && (j+n)<w) then 
            begin
		triangles_templist <- (j,i)::(triangles_templist);
		triangles_templist <- (j+n,i)::(triangles_templist);
		triangles_templist <- (j,i+n)::(triangles_templist);
            end;
	  if((i-n)>=0 && (j-n)>=0) then
            begin
		triangles_templist <- (j,i)::(triangles_templist);
		triangles_templist <- (j-n,i)::(triangles_templist);
		triangles_templist <- (j,i-n)::(triangles_templist);
            end
	end
      done
    done


method private clean_point_list = if ((List.length points_list) > 0) then
    (points_list <- []; triangles_templist <- [])

method private image2grill img n = this#clean_point_list; this#make_trianglelist img n;
  let (w,h) = this#get_dims img in
    let grill image j i =
      begin
	      if(Sdlvideo.get_pixel_color image j i = (0,0,0)) then
		points_list <- this#add_if_new (j,i) (points_list);
	      Sdlvideo.put_pixel_color image j i (0,0,0);
      end in
        for i=0 to (h-1) do
          for j=0 to (w-1) do
            if (j mod n = i mod n) then grill img j i;
            if(i mod n = 0) then  grill img j i;
           if(j mod n = 0) then grill img j i;
          done;
        done ;
        print_endline (string_of_int (List.length
           points_list))
  
     
method createGrill filepath n =
  begin
    let img = this#loadImage filepath in
      this#image2grill img n;
      this#saveImage img (filepath^".grille.bmp");
  end
   
end 
(* init de SDL *)
let sdl_init () =
  begin
    Sdl.init [`EVERYTHING];
    Sdlevent.enable_events Sdlevent.all_events_mask;
  end
    
(* Initialisation de SDL *)
let _ = sdl_init ();
