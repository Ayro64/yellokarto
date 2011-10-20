#directory "+lablGL";;
#load "unix.cma";;
#load "lablgl.cma";;
#load "lablglut.cma";;

let time =
  let start = Unix.gettimeofday () in
  (fun () -> Unix.gettimeofday () -. start)

let () =
  ignore (Glut.init Sys.argv);
  Glut.initDisplayMode ~double_buffer:true ();
  Glut.initWindowSize ~w:512 ~h:512;
  ignore (Glut.createWindow ~title:"OpenGL Demo");
  GlClear.color (0.1, 0.3, 0.1);
  GlDraw.shade_model `smooth;
  let render () =
    GlMat.mode `projection;
    GlMat.load_identity ();
    GluMat.ortho2d ~x:(-1.5, 1.5) ~y:(-1.5, 1.5);
    GlMat.mode `modelview;
    GlMat.load_identity ();
    GlClear.clear [ `color ];
    GlMat.rotate ~angle:(time () *. 150.) ~z:1. ();
    GlDraw.begins `triangles;
    List.iter GlDraw.vertex2 [-1., -1.; 0., 1.; 1., -1.];
    GlDraw.ends ();
    Gl.flush ();
    Glut.swapBuffers () in
  Glut.displayFunc ~cb:render;
  Glut.idleFunc ~cb:(Some Glut.postRedisplay);
  Glut.keyboardFunc ~cb:(fun ~key ~x ~y -> if key=27 then exit 0);
  Glut.mainLoop ()
