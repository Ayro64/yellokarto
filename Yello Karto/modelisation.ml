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

(* canal d'entree pour recuperer les points de la map 2d*)
let in_channel_points = open_in "points.txt"

(*canal d'entree pour recuperer la hauteur relative a chaque couleur*)
let in_channel_color = open_in "color.txt"

(*Creation du fichier .obj a partir d'une liste de triples d'entiers BON*)
let rec makeobj l ()= match l with
 | [] -> close_out out_channel
 | (x,y,z)::l -> begin
   	      	 output_string out_channel ("v "^(string_of_int x)^".0"^" "^(string_of_int y)^".0"^" "^(string_of_int z)^".0"^"\n");
       	         makeobj l ();
		end

let string_of_char = String.make 1

(*conversion d'une ligne de texte en couple d'entiers*)
let line_to_point line=
   let i = ref 0 and a = ref 0 and b = ref 0 in
      while(line.[!i] <> '-') do
	a := 10*(!a) + (int_of_string(string_of_char (line.[!i])));
	i := !i+1;
      done;
	i := !i+1;
      while(!i < String.length line) do
	b := 10*(!b) + (int_of_string(string_of_char (line.[!i])));
	i := !i+1;
      done;
   (!a,!b)

let rec string_to_pointlist = function
  | [] -> []
  | e::l -> (line_to_point e)::(string_to_pointlist l)

(*conversion du fichier contenant les points en liste de points*)
let textfile_to_pointlist =
  let lines = ref [] in
    try
     while true; do
	let a = input_line in_channel_points in
      	lines := a::(!lines);
	done; []
     with End_of_file -> close_in in_channel_points;
    (string_to_pointlist !lines)

let line_to_color line=
   let i = ref 0 and r = ref 0 and g = ref 0 and b = ref 0 and h = ref 0 in
      while(line.[!i] <> ' ') do
	r := 10*(!r) + (int_of_string(string_of_char (line.[!i])));
	i := !i+1;
      done;
	i := !i+1;
      while(line.[!i] <> ' ') do
	g := 10*(!g) + (int_of_string(string_of_char (line.[!i])));
	i := !i+1;
      done;
	i := !i+1;
      while(line.[!i] <> ' ') do
	b := 10*(!b) + (int_of_string(string_of_char (line.[!i])));
	i := !i+1;
      done;
	i := !i+1;
      while(!i < String.length line) do
	h := 10*(!h) + (int_of_string(string_of_char (line.[!i])));
	i := !i+1;
      done;
	(!r,!g, !b, !h)

let rec string_to_colorlist = function
  | [] -> []
  | e::l -> (line_to_color e)::(string_to_colorlist l)


let createcolorlist =
  let lines = ref [] in
    try
     while true; do
       let a = input_line in_channel_color in
       lines := a::(!lines);
     done; []
    with End_of_file -> close_in in_channel_color;
    (string_to_colorlist !lines)

let twopto3p coord2d colorlist img =
  let (x,y) = coord2d in
    let (r,g,b) = (Sdlvideo.get_pixel_color img x y) in
    let rec tptotp coord2d colorlist (r,g,b) = match (coord2d,colorlist, (r,g,b)) with
      | ((x,y),((r1,g1,b1,h)::l),(r2,g2,b2)) -> 
		if (r1 = r2 && g1 = g2 && b1 = b2) then (x,y,h)
		else tptotp (x,y) l (r2,g2,b2);
      | _ -> (0,0,0)
    in tptotp coord2d colorlist (r,g,b)

let dualtotriple img pointlist = 
    let colorlist = createcolorlist in
	let rec d2t pl = match pl with
	  | [] -> []
	  | e::l -> (twopto3p e colorlist img)::(d2t l)
	in d2t pointlist

(*creation du fichier obj*)
let create_obj_file img =
  let l = textfile_to_pointlist in
    let threepointlist = dualtotriple img l in
	makeobj threepointlist ()

(* Dimensions d'une image *)
let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)

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
          color_list := add_if_new (Sdlvideo.get_pixel_color img i j) !color_list;
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
