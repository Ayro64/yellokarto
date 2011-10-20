#directory "+lablGL"
#load "unix.cma"
#load "lablgl.cma"
#load "lablglut.cma"

let render () =
    GlClear.clear [ `color ];
    GlDraw.begins `triangles;
    List.iter GlDraw.vertex2 [-1., -1.; 0., 1.; 1., -1.];
    (*List.iter GLDraw.vertex2 [-1., -1.; 0., 1.; 1., -1.];*)
    GlDraw.ends ();
    Gl.flush ();
    Glut.swapBuffers ();;

let () =
  ignore (Glut.init Sys.argv);
  Glut.initDisplayMode ~double_buffer:true ();
  Glut.initWindowSize ~w:512 ~h:512;
  ignore (Glut.createWindow ~title:"OpenGL Demo");
  GlClear.color (0.1, 0.3, 0.1);
  Glut.displayFunc ~cb:render;
  Glut.mainLoop ()
