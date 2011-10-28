let memo f =
  let h = Hashtbl.create 101 in (* on choisit un nombre premier adéquate *)
    fun x ->
      try Hashtbl.find h x with
      Not_found ->
        let y = f x in
          Hashtbl.add h x y;
          y

(* opérateur de point fixe *)
let mu f =
  let rec aux x = f aux x in
    aux
 
(* factorielle par contination *)
let cont_fact fact = function
    0|1 -> 1
  | n -> n * fact (n-1)
 
(* application *)
let x = mu cont_fact 5



(* val memo : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b *)
