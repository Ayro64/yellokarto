(*let out_channel_map = open_out "map3d.obj" *)
let in_channel_map = open_in "map3d.obj"
  
let string_of_char = String.make 1
  
let line2p line =
  let i = ref 2 and x = ref 0. and y = ref 0. and z = ref 0. and s = ref "" in
    while(line.[!i] <> ' ') do
      s := !s^(string_of_char (line.[!i]));
      i := !i+1;
    done;
    x := float_of_string !s;
    s := "";
    i := !i+1;
    while(line.[!i] <> ' ') do
      s := !s^(string_of_char (line.[!i]));
      i := !i+1;
    done;
    y := float_of_string !s;
    s := "";
    i := !i+1;
    while(!i < String.length line) do
      s := !s^(string_of_char (line.[!i]));
      i := !i+1;
done;
    z := float_of_string !s;
    (!x, !y, !z)
      
let rec string2plist = function
  | [] -> []
  | e::l -> (line2p e)::(string2plist l)
      
      
let getpoints =
  let lines = ref [] in
    try
      while true; do
	let a = input_line in_channel_map in
	  lines := a::(!lines);
      done; []
    with End_of_file -> close_in in_channel_map;
      (string2plist !lines)


let time =
  let start = Unix.gettimeofday () in
    fun () -> Unix.gettimeofday () -. start

let rec iter = function
    [] -> ()
  | h::l -> GlDraw.vertex3 h; iter l

let initGL _ =
  GlMat.mode `projection;
  GlMat.load_identity ();
  GluMat.perspective ~fovy:45.0 ~aspect:(978./.470.) ~z:(0.1, 1000.);
  GluMat.look_at (600., 600., 600.) (0., 0., 0.) ( 0., 0., 1.);
  GlMat.mode `modelview;
  GlMat.load_identity ();
  GlClear.clear [`depth ; `color];
  Gl.enable `depth_test

let display3D _ =
  GlClear.clear [`depth ; `color];
  GlMat.load_identity ();
  GlMat.rotate ~angle:(50. *. time ()) ~x:0. ~y:0. ~z:0. ();

  GlDraw.begins `points;

  iter getpoints;
(*
  GlDraw.color (1., 0., 0.);
  GlDraw.vertex3 (1., 1., 1.);
  GlDraw.vertex3 (1., 1., -1.);
  GlDraw.vertex3 (-1., 1., -1.);
  GlDraw.vertex3 (-1., 1., 1.);

  GlDraw.color (0., 1., 0.);
  GlDraw.vertex3 (1., -1., 1.);
  GlDraw.vertex3 (1., -1., -1.);
  GlDraw.vertex3 (1., 1., -1.);
  GlDraw.vertex3 (1., 1., 1.);

  GlDraw.color (0., 0., 1.);
  GlDraw.vertex3 (-1., -1., 1.);
  GlDraw.vertex3 (-1., -1., -1.);
  GlDraw.vertex3 (1., -1., -1.);
  GlDraw.vertex3 (1., -1., 1.);

  GlDraw.color (0.8, 0.1, 0.89);
  GlDraw.vertex3 (-1., 1., -1.);
  GlDraw.vertex3 (1., 1., -1.);
  GlDraw.vertex3 (1., -1., -1.);
  GlDraw.vertex3 (-1., -1., -1.);
  
  GlDraw.color (0.7, 0.5, 0.12);
  GlDraw.vertex3 (-1., 1., 1.);
  GlDraw.vertex3 (-1., 1., -1.);
  GlDraw.vertex3 (-1., -1., -1.);
  GlDraw.vertex3 (-1., -1., 1.);

  GlDraw.color (0.05, 0.60, 1.);
  GlDraw.vertex3 (-1., 1., 1.);
  GlDraw.vertex3 (1., 1., 1.);
  GlDraw.vertex3 (1., -1., 1.);
  GlDraw.vertex3 (-1., -1., 1.);
*)  GlDraw.ends ();
  Gl.flush ()