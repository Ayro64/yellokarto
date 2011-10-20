#directory "+lablGL"
#directory "+sdl"
#load "unix.cma"
#load "lablgl.cma"
#load "lablglut.cma"
#load "sdl.cma"

type camera = {

  mutable sensibility:float;

  mutable x: float;
  mutable y: float;
  mutable z: float;

  mutable teta:float;
  mutable phi:float;

  mutable xdir: float;
  mutable ydir: float;
  mutable zdir: float;
}

let cam = {
  sensibility = 0.01;
  x = 50.;
  y = 50.;
  z = 500.;

  teta = 45.;
  phi = -45.;
  
  xdir = 0.;
  ydir = 0.;
  zdir = 0.;
}

let scene = ref []

(** timer which return the time since last call*)
let time =
    let start = Unix.gettimeofday () in
    fun () -> Unix.gettimeofday () -. start

let dt = ref 0.


(**return the normal to the face decribed by 3 points *)
let get_face_norm (x1,y1,z1) (x2,y2,z2) (x3,y3,z3) =
	let (x4,y4,z4) = (x3-.x1,y3-.y1,z3-.z1) in
	let (x5,y5,z5) = (x2-.x1,y2-.y1,z2-.z1) in
	(1.*.(y4*.z5-.z4*.y5), 1.*.(z4*.x5-.x4*.z5), 1.*.(x4*.y5-.y4*.x5))

(**Update the camera forward with the angles*)
let get_forward () =
	if cam.phi > 89. then cam.phi <- 89.;
	if cam.phi < -89. then cam.phi <- -89.;
	let pi =  3.14 in
	let r_temp = cos (cam.phi*.pi/.180.) in
	cam.zdir <- sin (cam.phi*.pi/.180.);
    	cam.xdir <- r_temp *. cos (cam.teta*.pi/.180.);
    	cam.ydir <- r_temp *. sin (cam.teta*.pi/.180.)

(**return the left vector of the camera*)
let get_left () =
	(-1.0*.cam.ydir, cam.xdir, 0.)

(**Initialize the light in the 3D scene*)
let init_lighting () = 
    Gl.enable `lighting;
    Gl.enable `light0;
    Gl.enable `normalize;

    GlLight.light ~num:0 (`position (500000., 500000., 500000., 1.));
    GlLight.light ~num:0 (`diffuse (0.9, 0.9, 0.9, 1.));

    Gl.enable `color_material;
    GlLight.material `both (`specular (1.,1.,1.,1.));
    GlLight.material `both (`shininess 50.);
    GlLight.material `both (`diffuse (0.9,0.9,0.9,1.))

(**Update the camera in the scene*)
let cam_look () = 
	get_forward ();
	GluMat.look_at (cam.x, cam.y, cam.z) (cam.x +. cam.xdir, cam.y +. cam.ydir, cam.z +. cam.zdir) (0., 0., 1.)

(** Initialize lablGL *)
let init_gl width height =
    GlDraw.shade_model `smooth;
    GlClear.color (0.0, 0.0, 0.0);
    GlClear.depth 1.0;
    GlClear.clear [`color; `depth];
    Gl.enable `depth_test;
    init_lighting ();
    GlFunc.depth_func `lequal

(**return the display list coressponding to the OBJ*)
let gen_obj_disp_list name = 
  let map = Objection.loadOBJ name in
  let vertexArray = map.Objection.verticeArray in
  let indiceArray = map.Objection.indexArray in
  let id =GlList.create `compile in
  GlList.begins id `compile;
  
  GlDraw.begins `triangles;
  for i = 0 to Array.length indiceArray-1 do
    begin
      let (i1,i2,i3) = indiceArray.(i) in
	begin
	  let face_norm = get_face_norm vertexArray.(i1) vertexArray.(i2) vertexArray.(i3) in
	  GlDraw.normal3 face_norm;
	  GlDraw.vertex3 vertexArray.(i1);
	  GlDraw.vertex3 vertexArray.(i2);
	  GlDraw.vertex3 vertexArray.(i3);
	end
      end
    done;
  GlDraw.ends ();
  GlList.ends ();
  id 

(**OpenGL main draw loop*)
let draw_gl_scene () =
  GlClear.clear [`color; `depth];
  GlMat.load_identity();
  cam_look ();
  if !scene <> [] then
  begin
  	let map_list::l = !scene in
  	GlList.call map_list;
  end;
  
  Gl.flush();
  dt := time ();
  Glut.swapBuffers ()
  

(** Handle window reshape events *)
let reshape_cb ~w ~h =
  let 
    ratio = (float_of_int w) /. (float_of_int h) 
  in
    GlDraw.viewport 0 0 w h;
    GlMat.mode `projection;
    GlMat.load_identity ();
    GluMat.perspective 60.0 ratio (1., 10000.0);
   
    GlMat.mode `modelview;
    GlMat.load_identity();
    GluMat.look_at (cam.x, cam.y, cam.z) (cam.x +. cam.xdir, cam.ydir, 50.) (0., 0., 1.)


(** Handle keyboard events *)
let keyboard_cb ~key ~x ~y =
  match key with
    | 122 -> cam.x <- cam.x +. cam.xdir *. !dt;
	     cam.y <- cam.y +. cam.ydir *. !dt;
	     cam.z <- cam.z +. cam.zdir *. !dt
    | 115 -> cam.x <- cam.x -. cam.xdir *. !dt;
	     cam.y <- cam.y -. cam.ydir *. !dt;
	     cam.z <- cam.z -. cam.zdir *. !dt
    | 113 -> let (x,y,z) = get_left () in
	     cam.x <- cam.x +. x *. !dt;
	     cam.y <- cam.y +. y *. !dt;
	     cam.z <- cam.z +. z *. !dt
    | 100 -> let (x,y,z) = get_left () in
	     cam.x <- cam.x -. x *. !dt;
	     cam.y <- cam.y -. y *. !dt;
	     cam.z <- cam.z -. z *. !dt
    | 108 -> GlDraw.polygon_mode `both `line;
	     Gl.disable `lighting;
       	     Gl.disable `light0;
	     Gl.disable `color_material
    | 102 -> GlDraw.polygon_mode `both `fill;
	     Gl.enable `lighting;
             Gl.enable `light0;
	     Gl.enable `color_material

    | 27 -> exit 0
    | _ -> Printf.printf "%i\n" key; ()

(**handle mouse motion event*)
let mouse_cb ~x ~y =
  let width = 640 in
  let height = 480 in
  let dx = (float_of_int (x-(width/2))) in
  let dy = (float_of_int (y-(height/2))) in
  if (dx <> 0.) && (dy == 0.) then
  begin
	cam.teta <- cam.teta -. dx *. cam.sensibility *. !dt;
	Glut.warpPointer (width/2) y;
  end;
  if (dy <> 0.) && (dx == 0.) then
  begin
	cam.phi <- cam.phi -. dy *. cam.sensibility *. !dt;
	Glut.warpPointer x (height/2);
  end;
  if (dy <> 0.) && (dx <> 0.) then
  begin
  	cam.teta <- cam.teta -. dx *. cam.sensibility *. !dt;
  	cam.phi <- cam.phi -. dy *. cam.sensibility *. !dt;
  	Glut.warpPointer (width/2) (height/2);
  end;
  ()
 
(**call the glut display every 33 ms*)
let rec timer value =
  Glut.postRedisplay ();
  Glut.timerFunc ~ms:33
                 ~cb:(fun ~value:x -> (timer x))
                 ~value:value

(**launch the 3D rendering of the nam obj*)
let launc3d name =
  let 
    width = 640 and
    height = 480
  in
    ignore (Glut.init Sys.argv);
    Glut.initDisplayMode ~alpha:true ~depth:true ~double_buffer:true ();
    Glut.initWindowSize width height;
    ignore (Glut.createWindow "VlaLeTopo");
    Glut.displayFunc draw_gl_scene;
    Glut.keyboardUpFunc keyboard_cb;
    Glut.reshapeFunc reshape_cb;
    Glut.passiveMotionFunc mouse_cb;
    Glut.timerFunc ~ms:33
                   ~cb:(fun ~value:x -> (timer x)) 
                   ~value:33;
    init_gl width height;
    scene :=  (gen_obj_disp_list name)::[];
    Glut.mainLoop ()





(* type cameraType = *)
(* { *)
(*   mutable hold : bool; *)
(*   mutable angleY : int; *)
(*   mutable angleZ : int; *)
(*   mutable distance : int; *)
(*   motionSensivity : float; *)
(*   scrollSensivity : float; *)

(*   let camera = *)
(*     if hold then  *)
(*       angleZ <- angleZ + SDL.event.xrel * motionSensivity;  *)
(*     angleY <- angleY + SDL.event.yrel * motionSensivity; *)
(*     if angleY > 90 then  *)
(*       angleY = 90 *)
(*     else if angleY < -90 then  *)
(*       angleY = -90 *)
(* ;; *)
(* };; *)


