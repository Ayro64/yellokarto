(* #directory "+lablGL" *)
(* #load "lablgl.cma" *)
(* #load "lablglut.cma" *)
#load "unix.cma"

let time =
  let start = Unix.gettimeofday () in
  fun () -> Unix.gettimeofday () -. start

let file = open_in "coor.obj" in
let tab = Array.make 8 "" in
  for i = 0 to 7 do
  Array.set tab i (input_line file)
  done


let display () =
  GlMat.mode `projection;
  GlMat.load_identity ();
  GluMat.perspective ~fovy:45.0 ~aspect:1. ~z:(0.1, 1000.);
  GluMat.look_at (3., 4., 2.) (0., 0., 0.) ( 0., 0., 1.);
  GlMat.mode `modelview;
  GlMat.load_identity ();
  GlClear.clear [`depth ; `color];
  Gl.enable `depth_test;

  GlMat.rotate ~angle:(-100. *. time ()) ~x:1. ~y:1. ~z:0. ();

  GlDraw.begins `quads;

  GlDraw.color (1., 0., 0.);
  GlDraw.vertex3 (1., 1., 1.);
  GlDraw.vertex3 (1., 1., -1.);
  GlDraw.color (0., 1., 0.);
  GlDraw.vertex3 (-1., 1., -1.);
  GlDraw.vertex3 (-1., 1., 1.);

  GlDraw.color (0., 1., 0.);
  GlDraw.vertex3 (1., -1., 1.);
  GlDraw.vertex3 (1., -1., -1.);
  GlDraw.color (0.57, 0.10, 1.);
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
  GlDraw.color (0., 1., 0.);
  GlDraw.vertex3 (-1., -1., 1.);

  GlDraw.ends ();

  Gl.flush ();
  Glut.swapBuffers ();;

let _ = 
  let _ = Glut.init Sys.argv in
  Glut.initDisplayMode ~depth:true ~double_buffer:true ();
  Glut.initWindowSize ~w:512 ~h:512;
  ignore(Glut.createWindow ~title:"Annuli OpenGL demo");
  Glut.displayFunc ~cb:display;
  Glut.idleFunc ~cb:(Some Glut.postRedisplay);
  Glut.keyboardFunc ~cb:(fun ~key ~x ~y -> if key=27 then exit 0);
  Glut.mainLoop ();
