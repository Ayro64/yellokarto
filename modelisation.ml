let loadImage file =
  Sdlloader.load_image file
    
let saveImage surface file =
  Sdlvideo.save_BMP surface file
    
let sumRGB (r,g,b) = r + g + b
  
let show img dst =
  let d = Sdlvideo.display_format img in
    Sdlvideo.blit_surface d dst ();
    Sdlvideo.flip dst
      
(* canal de sortie pour la creation/ecriture du fichier obj*)
let out_channel = open_out "map3d.obj"

let list_points = ref []
let list_triangles = ref []
let colours_list = ref []

let create_height (r,g,b,h) = colours_list := (r,g,b,h)::(!colours_list)

(*Creation du fichier .obj a partir d'une liste de triples d'entiers BON*)
let rec makeobj l () = match l with
  | [] -> close_out out_channel
  | (x,y,z)::l -> begin
      output_string out_channel 
	("v "^ (string_of_int x)^" "^
	   (string_of_int y)^" "^
	   (string_of_int z)^"\n");
      makeobj l ();
    end
(*      
let string_of_char = String.make 1
*)  
let to_pointlist () =
     list_points := !Traitement_image.points_list;
      !list_points

let to_trianglelist () =
     list_triangles := !Traitement_image.triangles_templist;
      !list_triangles


(* conversion d'un point2D en (coord3d, color) *)

let twopto5p coord2d colorlist img =
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

let dualtoxyzrgb img pointlist =
    let rec d2t pl = match pl with
    | [] -> []
    | e::l -> (twopto5p e !colours_list img)::(d2t l)
in d2t pointlist


let twopto3p coord2d colorlist img =
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
       
let dualtotriple img pointlist =
    let rec d2t pl = match pl with
    | [] -> []
    | e::l -> (twopto3p e !colours_list img)::(d2t l)
in d2t pointlist
       

let create_obj_file filepath =
  let img = loadImage filepath in
  let l = to_pointlist () in
  let threepointlist = dualtotriple img l in
    makeobj threepointlist ()

let getfulltriangles filepath =
  let img = loadImage filepath in
  let l = to_trianglelist () in
  let completepointlist = dualtoxyzrgb img l in
	completepointlist

(* Dimensions d'une image *)
let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, 
   (Sdlvideo.surface_info img).Sdlvideo.h)
    
let rec is_in_list elt list = match list with
  |[] -> false
  |e::l when e=elt -> true
  |e::l -> is_in_list elt l
     
let add_if_new elt list = match (elt, list) with
  |(x,l) when (not(is_in_list x l)) ->  x::l
  |(x,l) -> l
     
let list_colours img =
  let color_list = ref [] in
  let (w,h) = get_dims img in
    for i=0 to h-1 do
      for j=0 to w-1 do
        if ((Sdlvideo.get_pixel_color img i j) <> (0,0,0)) then
          (color_list := add_if_new (Sdlvideo.get_pixel_color img i j)
             !color_list);
      done
    done;
    !color_list
      
let get_list_colours filepath =
  begin
    let img = loadImage filepath in
      (*on applique la transformation*)
      list_colours img;
  end
    
(* init de SDL *)
let sdl_init () =
  begin
    Sdl.init [`EVERYTHING];
    Sdlevent.enable_events Sdlevent.all_events_mask;
  end
    
(* Initialisation de SDL *)
let _ = sdl_init ();
