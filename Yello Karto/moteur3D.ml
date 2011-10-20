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
	GlDraw.color (0.263, 0.654, 0.132);
	GlDraw.vertex3 (1., 1., -1.);
	GlDraw.color (0.3, 0.8, 0.2);
	GlDraw.vertex3 (-1., 1., -1.);
	GlDraw.color (0.154, 0.221, 0.665);
	GlDraw.vertex3 (-1., 1., 1.);
	
	GlDraw.color (0., 1., 0.);
	GlDraw.vertex3 (1., -1., 1.);
	GlDraw.color (0.456, 0.853, 0.667);
	GlDraw.vertex3 (1., -1., -1.);
	GlDraw.color (0.758, 0.247, 0.467);
	GlDraw.vertex3 (1., 1., -1.);
	GlDraw.color (0.587, 0.885, 0.21);
	GlDraw.vertex3 (1., 1., 1.);
	
	GlDraw.color (0., 0., 1.);
	GlDraw.vertex3 (-1., -1., 1.);
	GlDraw.vertex3 (-1., -1., -1.);
	GlDraw.color (0.738, 0.48, 0.83);
	GlDraw.vertex3 (1., -1., -1.);
	GlDraw.vertex3 (1., -1., 1.);
	
	GlDraw.color (0.8, 0.1, 0.89);
	GlDraw.vertex3 (-1., 1., -1.);
	GlDraw.vertex3 (1., 1., -1.);
	GlDraw.color (0.32, 0.425, 0.6);
	GlDraw.vertex3 (1., -1., -1.);
	GlDraw.vertex3 (-1., -1., -1.);
	
	GlDraw.color (0.7, 0.5, 0.12);
	GlDraw.vertex3 (-1., 1., 1.);
	GlDraw.vertex3 (-1., 1., -1.);
	GlDraw.color (0.6, 0.1, 0.2);
	GlDraw.vertex3 (-1., -1., -1.);
	GlDraw.vertex3 (-1., -1., 1.);
	
	GlDraw.color (0.05, 0.60, 1.);
	GlDraw.vertex3 (-1., 1., 1.);
	GlDraw.vertex3 (1., 1., 1.);
	GlDraw.color (0.415, 0.656, 0.32);
	GlDraw.vertex3 (1., -1., 1.);
	GlDraw.vertex3 (-1., -1., 1.);	
	GlDraw.ends ();	
	Gl.flush ()


