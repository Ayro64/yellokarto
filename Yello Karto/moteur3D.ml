let time =
  let start = Unix.gettimeofday () in
  fun () -> Unix.gettimeofday () -. start


let initGL _ =
	GlMat.mode `projection;
	GlMat.load_identity ();
	GluMat.perspective ~fovy:45.0 ~aspect:(800.0/.600.) ~z:(0.1, 1000.);
	GluMat.look_at (3., 4., 2.) (0., 0., 0.) ( 0., 0., 1.);
	GlMat.mode `modelview;
	GlMat.load_identity ();
	GlClear.clear [`depth ; `color];
	Gl.enable `depth_test
   

let display3D _ =
GlClear.clear [`depth ; `color];
	GlMat.rotate ~angle:(-100. *. time ()) ~x:1. ~y:1. ~z:0. ();
	
	GlDraw.begins `quads;
	
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
	GlDraw.ends ();	
	Gl.flush ()


