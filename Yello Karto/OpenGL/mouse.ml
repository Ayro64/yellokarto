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
  GlMat.load_identity ();
  GlClear.clear [`color];

  GlDraw.begins `points;
  GlDraw.vertex2 (!px, !py);
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
  Glut.mainLoop ();
