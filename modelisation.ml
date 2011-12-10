class modelisation traitement =
object(this)
  
  val traitement = traitement
  val out_channel_delaunay = open_out "map3dd.yk"
  val out_channel_simple = open_out "map3ds.yk"
    
  method private loadImage file =
    Sdlloader.load_image file
      
  method private saveImage surface file =
    Sdlvideo.save_BMP surface file
      
  method private sumRGB (r,g,b) = r + g + b
    
  method private show img dst =
    let d = Sdlvideo.display_format img in
      Sdlvideo.blit_surface d dst ();
      Sdlvideo.flip dst
	
  val mutable list_points = []
  val mutable list_triangles = []
  val mutable colours_list = []
    
  method create_height (r,g,b,h) = colours_list <- (r,g,b,h)::(colours_list)
    
  (*Creation du fichier .obj a partir d'une liste de triples d'entiers BON*)
  method private makeDelaunayFile l =
    match l with
      | [] -> close_out out_channel_delaunay
      | (x,y,z,r,g,b)::t -> print_string "! ";begin
	  output_string out_channel_delaunay
	    ((string_of_int x)^" "^
	       (string_of_int y)^" "^
	       (string_of_int z)^" "^
(string_of_int r)^" "^(string_of_int g)^" "^(string_of_int b)^"\n");
	  this#makeDelaunayFile t;
	end
	  
  method private makeSimpleTriangulationFile = function
    | [] -> close_out out_channel_simple
    | (x,y,z,r,g,b)::t -> begin
	output_string out_channel_simple
	  ((string_of_int x)^" "^(string_of_int y)^" "^(string_of_int z)^" "^
	     (string_of_int r)^" "^(string_of_int g)^" "^(string_of_int b)^"\n");
	this#makeSimpleTriangulationFile t;
      end
	
  method private to_pointlist =
    list_points <- traitement#get_points_list;
    list_points
      
  method private to_trianglelist =
    list_triangles <- traitement#get_triangles_templist;
    list_triangles
      
      
  (* conversion d'un point2D en (coord3d, color) *)
      
  method private twopto5p coord2d colorlist img =
    let (x,y) = coord2d in
    let (r,g,b) = (Sdlvideo.get_pixel_color img x y) in
    let rec tptotp coord2d colorlist (r,g,b) = match (coord2d,colorlist,
						      (r,g,b))
    with
      | ((x,y),((r1,g1,b1,h)::l),(r2,g2,b2)) ->
	  if (r1 = r2 && g1 = g2 && b1 = b2) then (x,y,h,r1,g1,b1)
	  else tptotp (x,y) l (r2,g2,b2);
      | _ -> (0,0,0,0,0,0)
    in tptotp coord2d colorlist (r,g,b)
	 
  method private dualtoxyzrgb img pointlist =
    let rec d2t pl = match pl with
      | [] -> []
      | e::l -> (this#twopto5p e colours_list img)::(d2t l)
    in d2t pointlist
	 
	 
  method private twopto3p coord2d colorlist img =
    let (x,y) = coord2d in
    let (r,g,b) = (Sdlvideo.get_pixel_color img x y) in
    let rec tptotp coord2d colorlist (r,g,b) = match (coord2d,colorlist,
						      (r,g,b))
    with
      | ((x,y),((r1,g1,b1,h)::l),(r2,g2,b2)) ->
	  if (r1 = r2 && g1 = g2 && b1 = b2) then (x,y,h)
	  else tptotp (x,y) l (r2,g2,b2);
      | _ -> (0,0,0)
    in tptotp coord2d colorlist (r,g,b)
	 
  method private dualtotriple img pointlist =
    let rec d2t pl = match pl with
      | [] -> []
      | e::l -> (this#twopto3p e colours_list img)::(d2t l)
    in d2t pointlist
	 
	 
  method create_Delaunay_file filepath =
    let img = this#loadImage filepath in
    let l = this#to_pointlist in
    let threepointlist = this#dualtoxyzrgb img l in
      print_endline (string_of_int (List.length l));
      this#makeDelaunayFile threepointlist

	
  method create_simple_triangulation_file filepath =
    let img = this#loadImage filepath in
    let l = this#to_trianglelist in
    let completepointlist = this#dualtoxyzrgb img l in
      this#makeSimpleTriangulationFile completepointlist
	
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
       
  val mutable color_list = []
    
  method private list_colours img =
    (* let color_list = ref [] in*)
    let (w,h) = this#get_dims img in
      for i=0 to h-1 do
	for j=0 to w-1 do
          if ((Sdlvideo.get_pixel_color img i j) <> (0,0,0)) then
            (color_list <- this#add_if_new (Sdlvideo.get_pixel_color img i j)
               color_list);
	done
      done;
      color_list
	
  method get_list_colours filepath =
    begin
      let img = this#loadImage filepath in
	(*on applique la transformation*)
	this#list_colours img;
    end
end
  
let sdl_init _ =
  begin
    Sdl.init [`EVERYTHING];
    Sdlevent.enable_events Sdlevent.all_events_mask;
  end
    
let _ = sdl_init ()
