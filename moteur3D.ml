let out_channel_map = open_out "map3d.obj"
let close_writeObj = close_out out_channel_map
let string_of_char = String.make 1

let maxHeight = ref 0.
let xcam = ref 0.
let zcam = ref 0.

let line2p line =
  let i = ref 2 and x = ref 0. and y = ref 0. and z = ref 0. and s = ref "" in
    while(line.[!i] <> ' ') do
      s := !s^(string_of_char (line.[!i]));
      i := !i+1;
    done;
  x := (float_of_string !s) -. 255.;
    s := "";
    i := !i+1;
    while(line.[!i] <> ' ') do
      s := !s^(string_of_char (line.[!i]));
      i := !i+1;
    done;
    y := (float_of_string !s) -. 255.;
    s := "";
    i := !i+1;
    while(!i < String.length line) do
      s := !s^(string_of_char (line.[!i]));
      i := !i+1;
    done;
    z := float_of_string !s;
    if !z > !maxHeight then maxHeight := !z;
    (!x, !y, !z)
      

let rec string2plist = function
  | [] -> []
  | e::l -> (line2p e)::(string2plist l)
    
let vertexlist = ref []  

let getpoints () =
  let in_channel_map = open_in "map3d.obj" in
  let lines = ref [] in 
  try
    while true; do
      let a = input_line in_channel_map in
      lines := a::(!lines);
    done; []
  with End_of_file -> close_in in_channel_map;
    vertexlist :=  (string2plist !lines); zcam := !maxHeight; xcam := !maxHeight; !vertexlist
   
let time =
  let start = Unix.gettimeofday () in
  fun () -> Unix.gettimeofday () -. start
    
    
let rec iter = function
  | [] -> ()
  | h::l -> GlDraw.vertex3 h; iter l
    
let initGL _ =
  getpoints ();
  maxHeight := !maxHeight +. 200.;
  GlMat.mode `projection;
  GluMat.perspective ~fovy:45.0 ~aspect:(978./.470.) ~z:(0.1, 2500.);
  GluMat.look_at (!xcam, !xcam, !zcam) (200., 200., 200.) ( 0., 0., 1.);
  GlMat.mode `modelview;
  GlMat.load_identity ();
  GlClear.clear [`depth ; `color];
  Gl.enable `depth_test

    
let map3d _ = iter !vertexlist
  
let enable_rotate = ref false
let set_rotate rotate = enable_rotate := rotate
  
let display3D _ =
  GlClear.clear [`depth ; `color];
  GlMat.load_identity ();
  if (!enable_rotate) then
    (GlMat.rotate ~angle:(50. *. time ()) ~x:0. ~y:0. ~z:1. ());

  GlDraw.begins `points;
  GlDraw.color (0., 0.858, 0.378);
  map3d ();
  GlDraw.ends ();
  
  Gl.flush ()

