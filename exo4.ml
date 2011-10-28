let memo f =
  let h = Hashtbl.create 101 in (* on choisit un nombre premier adéquate *)
    let rec aux x =
      try Hashtbl.find h x with
      Not_found ->
          let y = f aux x in
          Hashtbl.add h x y;
          y
    in aux

let cont_fact fact = function
     0 | 1 -> 1
    | n -> n * fact (n-1)

let cont_fibo next = function
     0 | 1 as x -> x
    | x -> next (x-1) + next (x-2)
 
let fibo = memo cont_fibo 5

module StringSet = Set.Make(String)

let get_succ x r =
  try Hashtbl.find r x with Not_found -> StringSet.empty

let extend r a b =
  Hashtbl.replace r a (StringSet.add b (get_succ a r))

let is_prime x = print_endline (string_of_int (x)); (2 + (2*(memo cont_fact (x-1)) mod x)) = x

let next_prime a =
    let n = ref a in 
    while (not(is_prime !n)) do
      n := !n + 1
    done;
    print_endline (string_of_int !n); !n

let _ = print_endline (string_of_int (next_prime 14))
