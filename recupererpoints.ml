let in_channel_map = open_in "map3d.obj"

let line2p line=
   let i = ref 2 and x = ref 0. and y = ref 0. and z = ref 0. and s = ref "" in
      while(line.[!i] <> ' ')
	s := !s^(string_of_char (line.[!i])));
	i := !i+1;
      done;
	x := float_of_string s;
	s := "";
	i := !i+1;
      while(line.[!i] <> ' ') do
	y := !s^(string_of_char (line.[!i])));
	s := "";
	i := !i+1;
      done;
	z := float_of_string s;
	s := "";
	i := !i+1;
      while(!i < String.length line) do
	s := !s^(string_of_char (line.[!i])));
	i := !i+1;
      done;
	z := float_of_string s;
	(!x, !y, !z)

let rec string2plist = function
  | [] -> []
  | e::l -> (line2p e)::(string2plist l)


let getpoints =
  let lines = ref [] in
    try
     while true; do
       let a = input_line in_channel_map in
       lines := a::(!lines);
     done; []
    with End_of_file -> close_in in_channel_map;
    (string2plist !lines)
