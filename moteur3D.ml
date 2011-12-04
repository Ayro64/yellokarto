let out_channel_map = open_out "map3d.obj"
let temp = open_out "triangles.txt"
let close_writeObj = close_out out_channel_map
let string_of_char = String.make 1
let triangles = ref []
let vertexlist = ref []  
let validate = ref None
let maxHeight = ref 0.
let xcam = ref 0.
let zcam = ref 0.
let xminimal = ref infinity
let xmaximal = ref neg_infinity
let yminimal = ref infinity
let ymaximal = ref neg_infinity
let z1 = ref 0.
let z2 = ref 0.
let z3 = ref 0.
let z4 = ref 0.
  
let rec ecrireTriangleTxt t () = match t with
  | [] -> close_out temp
  | [(x1, y1, z1);(x2, y2, z2);(x3, y3, z3)]::l -> begin
      output_string temp
	("pt1 "^ (string_of_float x1)^" "^(string_of_float y1)^" "^(string_of_float z1)^"\n"^
	   "pt2 "^ (string_of_float x2)^" "^(string_of_float y2)^" "^(string_of_float z2)^"\n"^
	   "pt3 "^ (string_of_float x3)^" "^(string_of_float y3)^" "^(string_of_float z3)^"\n\n"
	);
      ecrireTriangleTxt l ();
    end
      
      
let line2p line =
  let i = ref 2 and x = ref 0. and y = ref 0. and z = ref 0. and s = ref "" in
    while(line.[!i] <> ' ') do
      s := !s^(string_of_char (line.[!i]));
      i := !i+1;
    done;
    x := (float_of_string !s);
    if !x = 0. then x := 1.;
    if !x = 510. then x := 509.;
    if !x > !xmaximal then xmaximal := !x;
    if !x < !xminimal then xminimal := !x;
    s := "";
    i := !i+1;
    while(line.[!i] <> ' ') do
      s := !s^(string_of_char (line.[!i]));
      i := !i+1;
    done;
    y := (float_of_string !s);
    if !y = 0. then y := 1.;
    if !y = 510. then y := 509.;
    if !y > !ymaximal then ymaximal := !y;
    if !y < !yminimal then yminimal := !y;
    s := "";
    i := !i+1;
    while(!i < String.length line) do
      s := !s^(string_of_char (line.[!i]));
      i := !i+1;
    done;
    (* z := 0.; *)
    z := float_of_string !s;
    (* if !z > !maxHeight then maxHeight := !z; *)
    if !x = !xminimal && !y = !yminimal then z1 := !z;
    if !x = !xmaximal && !y = !yminimal then z2 := !z;
    if !x = !xminimal && !y = !ymaximal then z3 := !z;
    if !x = !xmaximal && !y = !ymaximal then z4 := !z;
    (!x, !y, !z)
      
      
let rec string2plist = function
  | [] -> []
  | e::l -> (line2p e)::(string2plist l)
      
      
let getpoints () =
  let in_channel_map = open_in "map3d.obj" in
  let lines = ref [] in
    try
      while true; do
    	let a = input_line in_channel_map in
    	  lines := a::(!lines);
      done; []
    with End_of_file -> close_in in_channel_map;
      vertexlist := (string2plist !lines);
      zcam := !maxHeight;
      xcam := !maxHeight;
      print_float !xminimal;
      print_newline ();
      print_float !xmaximal;
      print_newline ();
      print_float !yminimal;
      print_newline ();
      print_float !ymaximal;
      print_newline ();
      print_float !z1;
      print_newline ();
      print_float !z2;
      print_newline ();
      print_float !z3;
      print_newline ();
      print_float !z4;
      print_newline ();

      (* vertexlist := [(21., 100., 80.);(100., 410., 80.); *)
      (* 		 (500., 100., 0.);(310., 310., 0.) *)
      (* 		 (\* (321., 280., 0.);(360., 180., 50.); *\) *)
      (* 		 (\* (200., 280., 50.);(440., 380., 50.); *\) *)
      (* 		 (\* (489., 280., 50.);(240., 121., 50.); *\) *)
      (* 		 (\* (321., 481., 50.);(40., 413., 0.); *\) *)
      (* 		 (\* (10., 488., 50.);(25., 50., 0.) *\)]; *)

      (* xminimal := 1.; *)
      (* yminimal := 1.; *)
      (* xmaximal := 510.; *)
      (* ymaximal := 510.; *)

      !vertexlist
	
let time =
  let start = Unix.gettimeofday () in
    fun () -> Unix.gettimeofday () -. start;;






let distance (p1x, p1y, _) (p2x, p2y, _) =
  (p1x -. p2x) *. (p1x -. p2x) +. (p1y -. p2y) *. (p1y -. p2y)
    
let tegal t1 = function
  | [p11;p12;p13] ->
      begin
	match t1 with
	  | [p21;p22;p23] ->
              (
		(p11 = p21 && p12 = p22 && p13 = p23)
		|| (p11 = p21 && p12 = p23 && p13 = p22)
		|| (p11 = p22 && p12 = p21 && p13 = p23)
		|| (p11 = p22 && p12 = p23 && p13 = p21)
		|| (p11 = p23 && p12 = p21 && p13 = p22)
		|| (p11 = p23 && p12 = p22 && p13 = p21)
              )
	  | _ -> failwith "Ce n'est pas un triangle"
      end
  | _ -> failwith "Ce n'est pas un triangle"
      
let segal (a, b) t =
  let l = ref t in
  let comp a b = not (a = b) in
    begin
      l := List.filter (comp a) !l;
      l := List.filter (comp b) !l;
      List.length !l = 1;
    end
      
let orthogonal (x1, y1) (x2, y2) =
  let (mx, my) = ((x1 +. x2) /. 2., (y1 +. y2) /. 2.)
  and coef = -.(x2 -. x1) /. (y2 -. y1) in
  let sup = my -. mx *. coef in
    (coef, sup)
      
let rec circleCenter = function
  | [(t1x, t1y, _);(t2x, t2y, _);(t3x, t3y, _)] ->
      if t1y -.t2y = 0. && t2y -.t3y = 0.then
	failwith "Triangle plat"
      else if t2y -. t3y = 0. then
	circleCenter [(t2x,t2y,0.);(t3x,t3y,0.);(t1x,t1y,0.)]
      else if t1y -. t2y = 0. then
	let x = (t1x +. t2x) /. 2. in
	let (a, b) = orthogonal (t1x, t1y) (t3x, t3y) in
	  (x, a *. x +. b)
      else
	let (a1, b1) = orthogonal (t1x, t1y) (t2x, t2y)
	and (a2, b2) = orthogonal (t1x, t1y) (t3x, t3y) in
	let x = (b2 -. b1) /. (a1 -. a2) in
	  (x, a1 *. x +. b1)
  | _ -> failwith "Ce n'est pas un triangle"
      
let rec pointInCircle r (cx,cy) t = function
    [] -> None
  | ((px, py, _) as p)::_ when int_of_float r > int_of_float
      ((cx -. px) *. (cx -. px) +. (cy -. py) *. (cy -. py)) ->
      Some (p,t)
  | _::l -> pointInCircle r (cx,cy) t l
      
let lastPoint p1 p2 t =
  let l = ref t in
  let comp a b = a <> b in
    l := List.filter (comp p1) !l;
    l := List.filter (comp p2) !l;
    if List.length !l <> 1 then
      print_endline ("Triangle non complet " ^ string_of_int
		       (List.length !l));
    match !l with
	e::l -> e
      | _ -> print_endline "Triangle incorrect";
	  p1
	    
let rec inCircle r c t ts = function
    [] -> ()
  | [p1;p2;p3] as e::l when
      segal (p1,p2) t
      || segal (p2,p3) t
      || segal (p1,p3) t
      -> validate := pointInCircle r c e e;
	if !validate = None then
	  inCircle r c t ts l
	else if segal (p2,p3) t then
	  ts := (p2,p3,lastPoint p2 p3 t)
	else if segal (p1,p2) t then
	  ts := (p1,p2,lastPoint p1 p2 t)
	else if segal (p1,p3) t then
	  ts := (p1,p3,lastPoint p1 p3 t)
	else
	  print_endline "Err:no seg"
	    
  | e::l -> inCircle r c t ts l
      
let checkTriangle tc = function
    ([(x, y, _) as p1;p2;p3] as t) ->
      validate := None;
      let (cx, cy) as c = circleCenter t in
      let r = (cx -. x) *. (cx -. x) +. (cy -. y) *. (cy -. y) in
	if int_of_float (distance (cx,cy,0.) p1) <> int_of_float
	  (distance (cx,cy,0.) p2)
	  || int_of_float (distance (cx,cy,0.) p1) <> int_of_float
	  (distance (cx,cy,0.) p3) then	      
	    inCircle r c t tc !triangles;
	!validate
  | _ -> failwith "Ce n'est pas un triangle"
      
let addTriangle p1 p2 p3 =
  triangles := [p1;p2;p3]::!triangles
    
let upedSegment (x, y, _) (p1x, p1y, _) (p2x, p2y, _) =
  if x < max p1x p2x && x > min p1x p2x then
    let coef = (p2y -. p1y) /. (p2x -. p1x) in
    let sup = p1y -. p1x *. coef in
    let y2 = coef *. x +. sup in
      if y2 > y then
	0
      else
	1
  else
    0
      
let inTriangle p = function
    [p1;p2;p3] ->
      upedSegment p p1 p2
      + upedSegment p p1 p3
      + upedSegment p p2 p3
      = 1
  | _ -> failwith "Ce n'est pas un triangle"
      
(* findTriangleOf p reft tlist
   find the triangle who contain p in the tlist and set the triangle in reft.
   After, return the tlist without the triangle which contain p.*)
let rec findTriangleOf p tr = function
  | [] -> []
  | t::l when inTriangle p t ->
      tr := t;
      l
  | e::l -> e::findTriangleOf p tr l
      
let rec moveTor t1 t2 r = function
    [] -> []
  | e::l when tegal e t1 -> r := true;t2::l
  | e::l -> e::moveTor t1 t2 r l
      
let moveTo t1 t2 l =
  let r = ref false in
  let ret = moveTor t1 t2 r l in
    begin
      if !r = false then
	print_endline "False Search";
      ret;
    end
      
let rec segCheck = function
    [p1;p2;p3] when (p1 = p2 || p1 = p3 || p2 = p3) ->
      (*print_endline "Triangle plat"*)()
  | ([p1;p2;p3] as t) ->
      let ts = ref (p1,p2,p3) in
	begin
	  match (checkTriangle ts t) with
	    | None -> ()
	    | Some (p4,_) when (p1 = p4 || p2 = p4 || p3 = p4) ->
		(*print_endline "Triangle confondu"*)()
	    | Some (p4,t2) ->
		let (pa,pb,ps) = !ts in
		  begin
		    triangles := moveTo t [pa;ps;p4] !triangles;
		    segCheck [pa;ps;p4];
		    triangles := moveTo t2 [pb;ps;p4] !triangles;
		    segCheck [pb;ps;p4];
		  end
	end
  | _ -> failwith "Ce n'est pas un triangle"
      
let notEqualPoints pt =
  pt <> (!xminimal, !yminimal, !z1) &&
    pt <> (!xminimal, !ymaximal, !z3) &&
    pt <> (!xmaximal, !yminimal, !z2) &&
    pt <> (!xmaximal, !ymaximal, !z4)
    
let insertPoint () =
  while !vertexlist <> [] do
    let p = List.hd !vertexlist in
      if notEqualPoints p then
	let t = ref [(509., 509., 0.);(0., 509., 0.);(509., 0., 0.)] in
      	  triangles := findTriangleOf p t !triangles;
	  match !t with
	      [p1;p2;p3] ->
		begin
		  addTriangle p p1 p2;
		  segCheck [p;p1;p2];
		  addTriangle p p2 p3;
		  segCheck [p;p2;p3];
		  addTriangle p p1 p3;
		  segCheck [p;p1;p3];
		  vertexlist := List.tl !vertexlist;
		end;
	    | _ -> failwith "Ce n'est pas un triangle"
      else vertexlist := List.tl !vertexlist;
  done
    
    
let initGL _ =
  getpoints ();
  triangles := [];
  validate := None;
  z1 := 0.;
  z2 := 0.;
  z3 := 0.;
  z4 := 0.;
  
  (* addTriangle (0., 0., 0.) (0., 70., 0.) (70., 0., 0.); *)
  (* addTriangle (0., 70., 0.) (70., 0., 0.) (70., 70., 0.); *)
  
  
  (* addTriangle (1., 1., 0.) (510., 0., 0.) (0., 510., 0.); *)
  (* addTriangle (510., 0., 0.) (0., 510., 0.) (510., 510., 0.); *)
  
  (* addTriangle (1., 1., 0.) (1., 510., 0.) (510., 1., 0.); *)
  (* addTriangle (1., 510., 0.) (510., 1., 0.) (510., 510., 0.); *)
  
  xminimal := 1.;
  yminimal := 1.;
  
  addTriangle (!xminimal -. 1., !yminimal -. 1., !z1) (!xmaximal, !yminimal -. 1., !z2) (!xminimal -. 1., !ymaximal, !z3);
  addTriangle (!xmaximal, !yminimal -. 1., !z2) (!xminimal -. 1., !ymaximal, !z3) (!xmaximal, !ymaximal, !z4);
  
  (* addTriangle (!xminimal -. 5., !yminimal -. 5., !z1) (!xmaximal +. 5., !yminimal -. 5., !z2) (!xminimal -. 5., !ymaximal +. 5., !z3); *)
  (* addTriangle (!xmaximal +. 5., !yminimal -. 5., !z2) (!xminimal -. 5., !ymaximal +. 5., !z3) (!xmaximal +. 5., !ymaximal +. 5., !z4); *)
  
  (* addTriangle (!xminimal,  !yminimal, !z1) (!xmaximal +. 2., *)
  (* 					    !yminimal, !z2) *)
  (*   (!xminimal, !ymaximal +. 2., !z3); *)
  (* addTriangle (!xmaximal +. 2., !yminimal, !z2) (!xminimal, !ymaximal *)
  (* 						   +. 2., !z3) *)
  (*   (!xmaximal +. 2., !ymaximal +. 2., !z4); *)
  
  insertPoint ();
  ecrireTriangleTxt !triangles ();
  
  (* maxHeight := !maxHeight +. 200.; *)
  GlMat.mode `projection;
  GluMat.perspective ~fovy:45.0 ~aspect:(978./.470.) ~z:(0.1, 2500.);
  (* GluMat.look_at (!xcam, !xcam, !zcam) (200., 200., 200.) ( 0., 0., 1.); *)
  GluMat.look_at (500., 500., 400.) (0., 0., 0.) ( 0., 0., 1.);
  GlMat.mode `modelview;
  GlMat.load_identity ();
  
  GlClear.clear [`depth ; `color];
  Gl.enable `depth_test;
  ()
    
let chooseColor = function
  | 0. -> GlDraw.color (0., 0., 1.)
  | 50. -> GlDraw.color (0., 1., 0.)
  | 100. -> GlDraw.color (0., 0.8, 0.)
  | 150. -> GlDraw.color (0., 0.6, 0.)
  | _ -> GlDraw.color (0., 0.4, 0.)
     
let rec iter xrefer yrefer = function
  | [] -> ()
  | [(x1, y1, z1);(x2, y2, z2);(x3, y3, z3)]::l ->
      (* GlDraw.vertex3 (x1 -. xrefer, y1 -. yrefer, z1); *)
      (* GlDraw.vertex3 (x2 -. xrefer, y2 -. yrefer, z2); *)
      (* GlDraw.vertex3 (x1 -. xrefer, y1 -. yrefer, z1); *)
      (* GlDraw.vertex3 (x3 -. xrefer, y3 -. yrefer, z3); *)
      (* GlDraw.vertex3 (x2 -. xrefer, y2 -. yrefer, z2); *)
      (* GlDraw.vertex3 (x3 -. xrefer, y3 -. yrefer, z3); *)
      
      chooseColor z1;
      GlDraw.vertex3 (x1 -. xrefer, y1 -. yrefer, z1);
      chooseColor z2;
      GlDraw.vertex3 (x2 -. xrefer, y2 -. yrefer, z2);
      chooseColor z3;
      GlDraw.vertex3 (x3 -. xrefer, y3 -. yrefer, z3);

      (* GlDraw.color (1., 0., 0.); *)
      (* GlDraw.vertex3 (x1 -. xrefer, y1 -. yrefer, z1); *)
      (* GlDraw.color (0., 1., 0.); *)
      (* GlDraw.vertex3 (x2 -. xrefer, y2 -. yrefer, z2); *)
      (* GlDraw.color (0., 0., 1.); *)
      (* GlDraw.vertex3 (x3 -. xrefer, y3 -. yrefer, z3); *)

      iter xrefer yrefer l
  | _ -> failwith "error"
      
let map3d _ = let xrefer = !xmaximal /. 2. and yrefer = !ymaximal /. 2. in
  iter xrefer yrefer !triangles
    
let enable_triangles = ref true
let set_triangles triangles = enable_triangles := triangles
  
let display3D _ =
  GlClear.clear [`depth ; `color];
  GlMat.load_identity ();

  GlMat.rotate ~angle:(50. *. time ()) ~x:0. ~y:0. ~z:1. ();
  
  if(!enable_triangles) then GlDraw.begins `triangles
  else GlDraw.begins `lines;

  (* GlDraw.begins `lines; *)
  (* GlDraw.vertex3 (0., 0., 0.); *)
  (* GlDraw.vertex3 (100., 0., 0.); *)
  (* GlDraw.vertex3 (0., 0., 0.); *)
  (* GlDraw.vertex3 (0., 100., 0.); *)
  (* GlDraw.vertex3 (0., 0., 0.); *)
  (* GlDraw.vertex3 (0., 0., 100.); *)
  map3d ();
  GlDraw.ends ();
  
  Gl.flush ()
