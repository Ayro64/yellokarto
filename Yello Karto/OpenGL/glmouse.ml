#directory "+lablGL"
#load "unix.cma"
#load "lablgl.cma"
#load "lablglut.cma"

let px = ref 0.0
let py = ref 0.0

let min_refresh = 30

let x_min = -6.0 and x_max = 6.0
and y_min = -6.0 and y_max = 6.0
and z_min, z_max = -6.0, 60.0 ;;

let ldown = ref false


let display () =
  GlMat.mode `projection;
  GlMat.load_identity ();
  GluMat.perspective ~fovy:45.0 ~aspect:1. ~z:(0.1, 1000.);
  GluMat.look_at (3., 4., 2.) (0., 0., 0.) ( 0., 0., 1.);
  GlMat.mode `modelview;
  GlMat.load_identity ();
  GlClear.clear [`depth ; `color];
  Gl.enable `depth_test;

  GlDraw.begins `triangles;

  GLDraw.vertex2 !px !py;


  GlDraw.ends ();

  Gl.flush ();
  Glut.swapBuffers ();;


(* let display() = *)
(*   GlMat.mode `projection; *)
(*   GlMat.load_identity (); *)
(*   GluMat.perspective ~fovy:45.0 ~aspect:1. ~z:(0.1, 1000.); *)
(*   GluMat.look_at (3., 4., 2.) (0., 0., 0.) ( 0., 0., 1.); *)
(*   GlMat.mode `modelview; *)
(*   GlMat.load_identity (); *)
(*   GlClear.clear [`depth ; `color]; *)
(*   Gl.enable `depth_test; *)
(*   GlClear.clear [`color]; *)
(*   GlMat.load_identity (); *)

(*   GLDraw.begins `points; *)

(*   if !ldown *)
(*   then GLDraw.color (1.0, 0.0, 0.0) *)
(*   else GLDraw.color (1.0, 1.0, 0.0); *)

(*   GLDraw.vertex2 !px !py; *)

(*   GLDraw.ends (); *)

(*   GL.Flush (); *)
(*   Glut.SwapBuffers(); *)
(* ;; *)


let _ = 
  let _ = Glut.init Sys.argv in
    Glut.initDisplayMode  ~depth:true ~double_buffer:true ();
    Glut.initWindowSize ~w:512 ~h:512;
    Glut.initWindowPosition ~x:100 ~y:100;

    ignore(Glut.createWindow ~title:"Annuli OpenGL demo");

    (* Glut.setCursor GLUT_CURSOR_NONE *)

    Glut.displayFunc ~display;
    Glut.reshapeFunc ~reshape;
    Glut.idleFunc ~idle;
    Glut.mouseFunc ~mouse;
    Glut.keyboardFunc ~keyboard;
    Glut.motionFunc ~motion;
    Glut.passiveMotionFunc ~passive;

  Glut.mainLoop ();
;;
