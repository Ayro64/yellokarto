class moteur3d =
object(this)
  
  val string_of_char = String.make 1
  val mutable triangles = []
  val mutable vertexlist = []  
  val mutable validate = None
  val mutable xminimal = infinity
  val mutable xmaximal = neg_infinity
  val mutable yminimal = infinity
  val mutable ymaximal = neg_infinity
  val mutable z1 =  0.
  val mutable z2 =  0.
  val mutable z3 =  0.
  val mutable z4 =  0.
  val mutable left_mouse_button_pressed = false
  val mutable right_mouse_button_pressed =false
  val mutable xold = 0
  val mutable yold = 0
  val mutable anglex = 0
  val mutable angley = 0
  val mutable altitude = 500
  val mutable enable_triangles = false

  val temp = open_out "triangles.txt"
    
  method set_triangles triangles = enable_triangles <- triangles

  (* method ecrireTriangleTxt t () = match t with
  | [] -> close_out temp
  | [(x1, y1, z1);(x2, y2, z2);(x3, y3, z3)]::l -> begin
      output_string temp
	("pt1 "^ (string_of_float x1)^" "^(string_of_float y1)^" "^(string_of_float z1)^"\n"^
	   "pt2 "^ (string_of_float x2)^" "^(string_of_float y2)^" "^(string_of_float z2)^"\n"^
	   "pt3 "^ (string_of_float x3)^" "^(string_of_float y3)^" "^(string_of_float z3)^"\n\n"
	);
      this#ecrireTriangleTxt l ();
    end *)
      
 
  method private line2p line =
    let i = ref 0 and x = ref 0. and y = ref 0. and z = ref 0. and
	r = ref 0. and g = ref 0. and b = ref 0. and
	s = ref "" in
      while(line.[!i] <> ' ') do
	s := !s^(string_of_char (line.[!i]));
	i := !i+1;
      done;
      x := (float_of_string !s);

      if !x > xmaximal then xmaximal <- !x;
      if !x < xminimal then xminimal <- !x;

      s := "";
      i := !i+1;
      while(line.[!i] <> ' ') do
	s := !s^(string_of_char (line.[!i]));
	i := !i+1;
      done;
      y := (float_of_string !s);

      if !y > ymaximal then ymaximal <- !y;
      if !y < yminimal then yminimal <- !y;

      s := "";
      i := !i+1;
      while(line.[!i] <> ' ') do
	s := !s^(string_of_char (line.[!i]));
	i := !i+1;
      done;
      z := (float_of_string !s);

      s := "";
      i := !i+1;
      while(line.[!i] <> ' ') do
	s := !s^(string_of_char (line.[!i]));
	i := !i+1;
      done;
      r := (float_of_string !s);

      s := "";
      i := !i+1;
      while(line.[!i] <> ' ') do
	s := !s^(string_of_char (line.[!i]));
	i := !i+1;
      done;
      g := (float_of_string !s);

      s := "";
      i := !i+1;
      while(!i < String.length line) do
	s := !s^(string_of_char (line.[!i]));
	i := !i+1;
      done;
      b := 0.;

      (* if !x = xminimal && !y = yminimal then z1 <- !z; *)
      (* if !x = xmaximal && !y = yminimal then z2 <- !z; *)
      (* if !x = xminimal && !y = ymaximal then z3 <- !z; *)
      (* if !x = xmaximal && !y = ymaximal then z4 <- !z; *)

      (!x, !y, !z, !r, !g, !b)
	
	
  method string2plist = function
    | [] -> []
    | e::l -> (this#line2p e)::(this#string2plist l)
	
	
  method getpoints =
    let in_channel_map = open_in "map3d.obj" in
    let lines = ref [] in
      try
	while true; do
    	  let a = input_line in_channel_map in
    	    lines := a::(!lines);
	done; []
      with End_of_file -> close_in in_channel_map;
	vertexlist <- (this#string2plist !lines);

	print_float xminimal;
	print_newline ();
	print_float xmaximal;
	print_newline ();
	print_float yminimal;
	print_newline ();
	print_float ymaximal;
	print_newline ();
	print_float z1;
	print_newline ();
	print_float z2;
	print_newline ();
	print_float z3;
	print_newline ();
	print_float z4;
	print_newline ();

	vertexlist


  method private distance (p1x, p1y, _, _, _, _) (p2x, p2y, _, _, _, _) =
    (p1x -. p2x) *. (p1x -. p2x) +. (p1y -. p2y) *. (p1y -. p2y)
      

  method private equalTriangles t1 = function
    | [p11;p12;p13] ->
	begin
	  match t1 with
	    | [p21;p22;p23] ->
              ((p11 = p21 && p12 = p22 && p13 = p23)
	       || (p11 = p21 && p12 = p23 && p13 = p22)
	       || (p11 = p22 && p12 = p21 && p13 = p23)
	       || (p11 = p22 && p12 = p23 && p13 = p21)
	       || (p11 = p23 && p12 = p21 && p13 = p22)
	       || (p11 = p23 && p12 = p22 && p13 = p21))
	    | _ -> failwith "Not a triangle"
	end
    | _ -> failwith "Not a triangle"
	

  method private segal (a, b) t =
    let l = ref t in
    let comp a b = not (a = b) in
      begin
	l := List.filter (comp a) !l;
	l := List.filter (comp b) !l;
	List.length !l = 1;
      end
	

  method private median (x1, y1) (x2, y2) =
    let (mx, my) = ((x1 +. x2) /. 2., (y1 +. y2) /. 2.)
    and coef = -.(x2 -. x1) /. (y2 -. y1) in
    let sup = my -. mx *. coef in
      (coef, sup)
	

  method private circleOfCenter = function
    | [(t1x, t1y, _, r1, g1, b1);(t2x, t2y, _,r2, g2, b2);(t3x, t3y, _, r3, g3, b3)] ->
	if t1y -.t2y = 0. && t2y -.t3y = 0.then
	  failwith "Flat triangle"
	else if t2y -. t3y = 0. then
	  this#circleOfCenter [(t2x, t2y, 0., r2, g2, b2);
			       (t3x, t3y, 0., r3, g3, b3);
			       (t1x, t1y, 0., r1, g1, b1)]
	else if t1y -. t2y = 0. then
	  let x = (t1x +. t2x) /. 2. in
	  let (a, b) = this#median (t1x, t1y) (t3x, t3y) in
	    (x, a *. x +. b)
	else
	  let (a1, b1) = this#median (t1x, t1y) (t2x, t2y)
	  and (a2, b2) = this#median (t1x, t1y) (t3x, t3y) in
	  let x = (b2 -. b1) /. (a1 -. a2) in
	    (x, a1 *. x +. b1)
    | _ -> failwith "Not a triangle"
	

  method private pointInCircle r (cx,cy) t = function
    |  [] -> None
    | ((px, py, _, _, _, _) as p)::_ when int_of_float r > int_of_float
	((cx -. px) *. (cx -. px) +. (cy -. py) *. (cy -. py)) ->
	Some (p,t)
    | _::l -> this#pointInCircle r (cx,cy) t l
	

  method private lastPoint p1 p2 t =
    let l = ref t in
    let comp a b = a <> b in
      l := List.filter (comp p1) !l;
      l := List.filter (comp p2) !l;
      if List.length !l <> 1 then
	print_endline ("Incomplete triangle");
      match !l with
	  e::l -> e
	| _ -> print_endline "Incorrect triangle";
	    p1


  method private inCircle r c t ts = function
    | [] -> ()
    | [p1;p2;p3] as e::l when
	this#segal (p1,p2) t
	|| this#segal (p2,p3) t
	|| this#segal (p1,p3) t
	-> validate <- this#pointInCircle r c e e;
	  if validate = None then
	    this#inCircle r c t ts l
	  else if this#segal (p2,p3) t then
	    ts := (p2,p3,this#lastPoint p2 p3 t)
	  else if this#segal (p1,p2) t then
	    ts := (p1,p2,this#lastPoint p1 p2 t)
	  else if this#segal (p1,p3) t then
	    ts := (p1,p3,this#lastPoint p1 p3 t)
	  else
	    print_endline "Err:no seg"	    
    | e::l -> this#inCircle r c t ts l
	

  method private checkTriangle tc = function
    | ([(x, y, _, _, _, _) as p1;p2;p3] as t) -> validate <- None;
	let (cx, cy) as c = this#circleOfCenter t in
	let r = (cx -. x) *. (cx -. x) +. (cy -. y) *. (cy -. y) in
	  if int_of_float (this#distance (cx, cy, 0., 42., 42., 42.)  1) 
	    <> int_of_float (this#distance (cx, cy, 0., 42., 42., 42.) p2)
	    || int_of_float (this#distance (cx, cy, 0., 42., 42., 42.) p1)
	    <> int_of_float (this#distance (cx, cy, 0., 42., 42., 42.) p3)
	  then this#inCircle r c t tc triangles;
	  validate
    | _ -> failwith "Not a triangle"
	

  method private addTriangle p1 p2 p3 =
    triangles <- [p1;p2;p3]::triangles
      

  method private upedSegment (x, y, _, _, _, _) (p1x, p1y, _, _, _, _) (p2x, p2y, _, _, _, _) =
    if x < max p1x p2x && x > min p1x p2x then
      let coef = (p2y -. p1y) /. (p2x -. p1x) in
      let sup = p1y -. p1x *. coef in
      let y2 = coef *. x +. sup in
	if y2 > y then 0
	else 1
    else 0
      

  method private inTriangle p = function
    | [p1;p2;p3] ->
	this#upedSegment p p1 p2 + this#upedSegment p p1 p3 + this#upedSegment p p2 p3 = 1
    | _ -> failwith "Not a triangle"
	

  method private findTriangleOf p tr = function
    | [] -> []
    | t::l when this#inTriangle p t -> tr := t; l
    | e::l -> e::this#findTriangleOf p tr l
	

  method private moveTor t1 t2 r = function
    | [] -> []
    | e::l when this#equalTriangles e t1 -> r := true;t2::l
    | e::l -> e::this#moveTor t1 t2 r l
	

  method private moveTo t1 t2 l =
    let r = ref false in
    let ret = this#moveTor t1 t2 r l in
      begin
	if !r = false then print_endline "False search";
	ret;
      end
	

  method private segCheck = function
    | [p1;p2;p3] when (p1 = p2 || p1 = p3 || p2 = p3) -> ()
    | ([p1;p2;p3] as t) ->
	let ts = ref (p1,p2,p3) in
	  begin
	    match (this#checkTriangle ts t) with
	      | None -> ()
	      | Some (p4,_) when (p1 = p4 || p2 = p4 || p3 = p4) -> ()
	      | Some (p4,t2) ->
		  let (pa,pb,ps) = !ts in
		    begin
		      triangles <- this#moveTo t [pa;ps;p4] triangles;
		      this#segCheck [pa;ps;p4];
		      triangles <- this#moveTo t2 [pb;ps;p4] triangles;
		      this#segCheck [pb;ps;p4];
		    end
	  end
    | _ -> failwith "Not a triangle"
	

  method private notEqualPoints (x, y, z, _, _, _) =
    let pt = (x, y, z) in
      pt <> (xminimal, yminimal, z1) &&
	pt <> (xminimal, ymaximal, z3) &&
	pt <> (xmaximal, yminimal, z2) &&
	pt <> (xmaximal, ymaximal, z4)
	

  method private insertPoint () =
    while vertexlist <> [] do
      let p = List.hd vertexlist in
	if this#notEqualPoints p then
	  let t = ref [(0., 0., 0., 42., 42., 42.);(0., 0., 0., 42., 42., 42.);(0., 0., 0., 42., 42., 42.)] in
      	    triangles <- this#findTriangleOf p t triangles;
	    match !t with
		[p1;p2;p3] ->
		  begin
		    this#addTriangle p p1 p2;
		    this#segCheck [p;p1;p2];
		    this#addTriangle p p2 p3;
		    this#segCheck [p;p2;p3];
		    this#addTriangle p p1 p3;
		    this#segCheck [p;p1;p3];
		    vertexlist <- List.tl vertexlist;
		  end;
	      | _ -> failwith "Not a triangle"
	else vertexlist <- List.tl vertexlist;
    done
      
      
  method private correction = function
    | [] -> []
    | (x, y, z, r, g, b)::t when x = xminimal ->
	(x +. 1., y, z, r, g, b)::this#correction t
    | (x, y, z, r, g, b)::t when x = xmaximal ->
	(x -. 1., y, z, r, g, b)::this#correction t
    | (x, y, z, r, g, b)::t when y = yminimal ->
	(x, y +. 1., z, r, g, b)::this#correction t
    | (x, y, z, r, g, b)::t when y = ymaximal ->
	(x, y -. 1., z, r, g, b)::this#correction t
      
      
  method initGL  =
    ignore(this#getpoints);
    vertexlist <- this#correction vertexlist;
    triangles <- [];
    validate <- None;
    z1 <- 0.;
    z2 <- 0.;
    z3 <- 0.;
    z4 <- 0.;
    (* addTriangle (0., 0., 0.) (0., 70., 0.) (70., 0., 0.); *)
    (* addTriangle (0., 70., 0.) (70., 0., 0.) (70., 70., 0.); *)
    
    
    (* addTriangle (1., 1., 0.) (510., 0., 0.) (0., 510., 0.); *)
    (* addTriangle (510., 0., 0.) (0., 510., 0.) (510., 510., 0.); *)
    
    (* addTriangle (1., 1., 0.) (1., 510., 0.) (510., 1., 0.); *)
    (* addTriangle (1., 510., 0.) (510., 1., 0.) (510., 510., 0.); *)
    
    (* xminimal <- 0.; *)
    (* yminimal <- 0.; *)
    
    this#addTriangle (xminimal, yminimal, z1, 0., 0., 0.) (xmaximal, yminimal, z2, 0., 0., 0.) (xminimal, ymaximal, z3, 0., 0., 0.);
    this#addTriangle (xmaximal, yminimal, z2, 0., 0., 0.) (xminimal, ymaximal, z3, 0., 0., 0.) (xmaximal, ymaximal, z4, 0., 0., 0.);
    
    (* this#addTriangle (0., 0., z1, 42., 42., 42.) (510., 0., z2, 42., 42., 42.) (0., 510., z3, 42., 42., 42.); *)
    (* this#addTriangle (510., 0., z2, 42., 42., 42.) (0., 510., z3, 42., 42., 42.) (510., 510., z4, 42., 42., 42.); *)
    
    this#insertPoint ();
    (* this#ecrireTriangleTxt triangles (); *)
    
    GlMat.mode `projection;
    GluMat.perspective ~fovy:45.0 ~aspect:(978./.470.) ~z:(0.1, 6500.);
    GlMat.mode `modelview;
    GlMat.load_identity ();    
    GlClear.clear [`depth ; `color];
    Gl.enable `depth_test;
    ()
      
      
  (* method private chooseColor = function *)
  (*   | 0. -> GlDraw.color (0., 0., 1.) *)
  (*   | x when x <= 50. -> GlDraw.color (0.5, 0.5, 0.) *)
  (*   | x when x > 50. && x <= 100. -> GlDraw.color (0.5, 0.5, 0.5) *)
  (*   | x when x > 100. && x <= 150.-> GlDraw.color (0.8, 0.5, 0.) *)
  (*   | x when x > 150. && x <= 200. -> GlDraw.color (0., 0.4, 0.) *)
  (*   | x when x > 200. && x <= 250. -> GlDraw.color (0., 0.6, 0.) *)
  (*   | x when x > 250. && x <= 300.-> GlDraw.color (0., 0.8, 0.) *)
  (*   | x when x > 300. && x <= 350.-> GlDraw.color (0., 1., 0.) *)
  (*   | _ -> GlDraw.color (1., 1., 0.) *)
	

  method private iter xrefer yrefer = function
    | [] -> ()
    | [(x1, y1, z1, r1, g1, b1);(x2, y2, z2, r2, g2, b2);
       (x3, y3, z3, r3, g3, b3)]::l ->
	begin
	  if(enable_triangles) then
	    begin
	      GlDraw.color (r1, g1, b1);
	      GlDraw.vertex3 (x1 -. xrefer, y1 -. yrefer, z1);
	      GlDraw.color (r2, g2, b2);
	      GlDraw.vertex3 (x2 -. xrefer, y2 -. yrefer, z2);
	      GlDraw.color (r3, g3, b3);
	      GlDraw.vertex3 (x3 -. xrefer, y3 -. yrefer, z3);
	    end
	  else
	    begin
	      GlDraw.vertex3 (x1 -. xrefer, y1 -. yrefer, z1);
	      GlDraw.vertex3 (x2 -. xrefer, y2 -. yrefer, z2);
	      GlDraw.vertex3 (x1 -. xrefer, y1 -. yrefer, z1);
	      GlDraw.vertex3 (x3 -. xrefer, y3 -. yrefer, z3);
	      GlDraw.vertex3 (x2 -. xrefer, y2 -. yrefer, z2);
	      GlDraw.vertex3 (x3 -. xrefer, y3 -. yrefer, z3);
	    end
	end;
	this#iter xrefer yrefer l
    | _ -> failwith "error"
	

  method private map3d = let xrefer = xmaximal /. 2. and
      yrefer = ymaximal /. 2. in this#iter xrefer yrefer triangles

    
  method mouse_pressed button state x y =  
    xold <- x;
    yold <- y;
    match button with
      | 1 -> left_mouse_button_pressed <- state
      | 3 -> right_mouse_button_pressed <- state
      | _ -> ()
	  
	  
  method motionMouse x y =
    if left_mouse_button_pressed then
      begin
	anglex <- anglex + (xold - x);
	angley <- angley + (yold - y);
      end;
    if right_mouse_button_pressed then
      altitude <- altitude + (yold - y);
    xold <- x;
    yold <- y
      
      
  method display3D =
    GlClear.clear [`depth ; `color];
    GlMat.load_identity ();    
    GluMat.look_at (float_of_int altitude, float_of_int altitude,
  		    float_of_int altitude) (0., 0., 0.) ( 0., 0., 1.);
    
    GlMat.rotate ~angle:(float(- angley)) ~x:0.0 ~y:1.0 ~z:0.0 ();
    GlMat.rotate ~angle:(float(- anglex)) ~x:0.0 ~y:0.0 ~z:1.0 ();
    
    if(enable_triangles) then GlDraw.begins `triangles
    else GlDraw.begins `lines;    
    this#map3d;  
    GlDraw.ends ();
    (* skybox#draw_skybox; *)
    Gl.flush ()      
end
