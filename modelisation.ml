let loadImage file =
      Sdlloader.load_image file

let saveImage surface file =
      Sdlvideo.save_BMP surface file

let sumRGB (r,g,b) = r + g + b


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
