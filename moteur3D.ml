let out_channel_map = open_out "map3d.obj"
let close_writeObj = close_out out_channel_map
let string_of_char = String.make 1

let maxHeight = ref 0.
let xcam = ref 0.
let zcam = ref 0.

let line2p line =
  let i = ref 2 and x = ref 0. and y = ref 0. and z = ref 0. and s = ref "" in
    while(line.[!i] <> ' ') do
      s := !s^(string_of_char (line.[!i]));
      i := !i+1;
    done;
  x := (float_of_string !s) -. 255.;
    s := "";
    i := !i+1;
    while(line.[!i] <> ' ') do
      s := !s^(string_of_char (line.[!i]));
      i := !i+1;
    done;
    y := (float_of_string !s) -. 255.;
    s := "";
    i := !i+1;
    while(!i < String.length line) do
      s := !s^(string_of_char (line.[!i]));
      i := !i+1;
    done;
    z := float_of_string !s;
    if !z > !maxHeight then maxHeight := !z;
    (!x, !y, !z)
      

let rec string2plist = function
  | [] -> []
  | e::l -> (line2p e)::(string2plist l)
    
let vertexlist = ref []  

let getpoints () =
  let in_channel_map = open_in "map3d.obj" in
  let lines = ref [] in 
  try
    while true; do
      let a = input_line in_channel_map in
      lines := a::(!lines);
    done; []
  with End_of_file -> close_in in_channel_map;
    vertexlist :=  (string2plist !lines); zcam := !maxHeight; xcam := !maxHeight; !vertexlist
   
let time =
  let start = Unix.gettimeofday () in
  fun () -> Unix.gettimeofday () -. start
    
    
let rec iter = function
  | [] -> ()
  | h::l -> GlDraw.vertex3 h; iter l
    
let initGL _ =
  getpoints ();
  maxHeight := !maxHeight +. 200.;
  GlMat.mode `projection;
  GluMat.perspective ~fovy:45.0 ~aspect:(978./.470.) ~z:(0.1, 2500.);
  GluMat.look_at (!xcam, !xcam, !zcam) (200., 200., 200.) ( 0., 0., 1.);
  GlMat.mode `modelview;
  GlMat.load_identity ();
  GlClear.clear [`depth ; `color];
  Gl.enable `depth_test

    
let map3d _ = iter !vertexlist
  
let enable_rotate = ref false
let set_rotate rotate = enable_rotate := rotate
  
let display3D _ =
  GlClear.clear [`depth ; `color];
  GlMat.load_identity ();
  if (!enable_rotate) then
    (GlMat.rotate ~angle:(50. *. time ()) ~x:0. ~y:0. ~z:1. ());

  GlDraw.begins `points;
  GlDraw.color (0., 0.858, 0.378);
  map3d ();
  GlDraw.ends ();
  
  Gl.flush ()











let distance (p1x, p1y, _) (p2x, p2y, _) =
  (p1x -. p2x) *. (p1x -. p2x) +. (p1y -. p2y) *. (p1y -. p2y)


let segal (a, b) t =
  let l = ref t in
  let comp a b = not (a = b) in
    begin
      l := List.filter (comp a) !l;
      l := List.filter (comp b) !l;
      List.length !l = 1;
    end

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


class delaunay =
object (this)
  val mutable points = []
  val mutable triangles = []
  val mutable validate = None
    
    
  method private orthogonal (x1, y1) (x2, y2) =
    let (mx, my) = ((x1 +. x2) /. 2., (y1 +. y2) /. 2.)
    and coef = -.(x2 -. x1) /. (y2 -. y1) in
    let sup = my -. mx *. coef in
    (coef, sup)

method private moveTor t1 t2 r = function
      [] -> []
    | e::l when tegal e t1 -> r := true;t2::l
    | e::l -> e::this#moveTor t1 t2 r l

  method private moveTo t1 t2 l =
    let r = ref false in
      let ret = this#moveTor t1 t2 r l in
      begin
  if !r = false then
    print_endline "False Search";
  ret;
    end

  method private circleCenter = function
    | [(t1x, t1y, _);(t2x, t2y, _);(t3x, t3y, _)] ->
  if t1y -.t2y = 0. && t2y -.t3y = 0.then
    failwith "Triangle plat"
  else if t2y -. t3y = 0. then
      this#circleCenter [(t2x,t2y,0.);(t3x,t3y,0.);(t1x,t1y,0.)]
    else if t1y -. t2y = 0. then
      let x = (t1x +. t2x) /. 2. in
      let (a, b) = this#orthogonal (t1x, t1y) (t3x, t3y) in
      (x, a *. x +. b)
    else
      let (a1, b1) = this#orthogonal (t1x, t1y) (t2x, t2y)
      and (a2, b2) = this#orthogonal (t1x, t1y) (t3x, t3y) in
      let x = (b2 -. b1) /. (a1 -. a2) in
      (x, a1 *. x +. b1)
    | _ -> failwith "Ce n'est pas un triangle"


  method private pointInCircle r (cx,cy) t = function
      [] -> None
    | ((px, py, _) as p)::_ when int_of_float r > int_of_float
    ((cx -. px) *. (cx -. px) +. (cy -. py) *. (cy -. py)) ->
  Some (p,t)
    | _::l -> this#pointInCircle r (cx,cy) t l

    
method private inCircle r c t ts = function
      [] -> ()
    | [p1;p2;p3] as e::l when
  segal (p1,p2) t
  || segal (p2,p3) t
  || segal (p1,p3) t
  -> validate <- this#pointInCircle r c e e;
    if validate = None then
      this#inCircle r c t ts l
    else if segal (p2,p3) t then
      ts := (p2,p3,this#lastPoint p2 p3 t)
    else if segal (p1,p2) t then
      ts := (p1,p2,this#lastPoint p1 p2 t)
    else if segal (p1,p3) t then
      ts := (p1,p3,this#lastPoint p1 p3 t)
    else
      print_endline "Err:no seg"

    | e::l -> this#inCircle r c t ts l


  method private lastPoint p1 p2 t =
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


  method addTriangle p1 p2 p3 =
    triangles <- [p1;p2;p3]::triangles

  method upedSegment (x, y, z) (p1x, p1y, p1z) (p2x, p2y, p2z) =
    if x < max p1x p2x && x > min p1x p2x then
      let coef = (p2y -. p1y) /. (p2x -. p1x) in
      let sup = p1y -. p1x *. coef in
      let y2 = coef *. x +. sup in
	if y2 > y then 0
	else 1
    else 0

  method inTriangle p = function
    | [p1;p2;p3] -> this#upedSegment p p1 p2 + this#upedSegment p p1 p3 +
	this#upedSegment p p2 p3 = 1
    | _ -> failwith "Not a triangle"

  method findTriangleOf p tr = function
    | [] -> []
    | t::l when this#inTriangle p t -> tr := t; l
    | e::l -> e::this#findTriangleOf p tr l

  method checkTriangle tc = function
      ([(x, y, _) as p1;p2;p3] as t) ->
	validate <- None;
	let (cx, cy) as c = this#circleCenter t in
	let r = (cx -. x) *. (cx -. x) +. (cy -. y) *. (cy -. y) in
	  if int_of_float (distance (cx,cy,0.) p1) <> int_of_float
	    (distance (cx,cy,0.) p2)
	    || int_of_float (distance (cx,cy,0.) p1) <> int_of_float
	    (distance (cx,cy,0.) p3) then
	      this#inCircle r c t tc triangles;
	  validate
    | _ -> failwith "Ce n'est pas un triangle"
	
	
  method segCheck = function
    | [p1;p2;p3] when (p1 = p2 || p1 = p3 || p2 = p3) -> ()
    | ([p1;p2;p3] as t) ->
	let ts = ref (p1, p2, p3) in
	begin
	  match (this#checkTriangle ts t) with
	    | None -> ()
	    | Some (p4,_) when (p1 = p4 || p2 = p4 || p3 = p4) ->
		()
	    | Some (p4,t2) ->
		let (pa,pb,ps) = !ts in
		  begin
		    triangles <- this#moveTo t [pa;ps;p4] triangles;
		    this#segCheck [pa;ps;p4];
		    triangles <- this#moveTo t2 [pb;ps;p4] triangles;
		    this#segCheck [pb;ps;p4];
		  end
	end
    | _ -> failwith "Ce n'est pas un triangle"
	
  method insertPoint () =
    while !vertexlist <> [] do
      let p = List.hd !vertexlist in
      let t = ref [(0., 0., 0.);(0., 0., 0.);(0., 0., 0.)] in
	triangles <- this#findTriangleOf p t triangles;
	match !t with
	    [p1;p2;p3] ->
	      begin
		this#addTriangle p p1 p2;
		this#segCheck [p;p1;p2];
		this#addTriangle p p2 p2;
		this#segCheck [p;p2;p3];
		this#addTriangle p p1 p3;
		this#segCheck [p;p1;p3];
		points <- List.tl points;
	      end;
	  | _ -> failwith "Not a triangle"
    done
end
