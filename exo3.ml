let memo f =
  let h = Hashtbl.create 101 in (* on choisit un nombre premier adéquate *)
    let rec aux x =
      try Hashtbl.find h x with
      Not_found ->
          let y = f aux x in
          Hashtbl.add h x y;
          y
    in aux


let cont_fibo next = function
     0 | 1 as x -> x
    | x -> next (x-1) + next (x-2)
 
let fibo = memo cont_fibo 5
