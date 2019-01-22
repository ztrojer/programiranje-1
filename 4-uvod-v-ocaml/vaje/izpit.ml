let rec zadnji list =
  match list with
  | [] -> failwith "list too short."
  | x :: [] -> x
  | x :: xs -> zadnji xs


let rec predzadnji list = 
  match list with
  | [] -> failwith "list too short."
  | x :: [] -> failwith "list too short."
  | x :: _ :: [] ->  x
  | x1 :: x2 :: xs -> predzadnji (x2 :: xs)


let rec k_ti_element k = function
  | [] -> failwith "list too short."
  | x :: xs when k <= 0 -> x
  | x :: xs ->  k_ti_element (k-1) xs

let rec podvoji_elemente = function
  | [] -> []
  | x :: [] -> [x ; x]
  | x :: xs -> [x ; x] @ podvoji_elemente xs

let rec razdeli k list =
  match k, list with
  | k, list when k <= 0 -> ([], list)
  | k, [] -> ([], [])
  | k, x :: xs ->
    let (left, right) = razdeli (k-1) xs in
    (x :: left, right)


let rec izbrisi k list =
  match k, list with
  | k, [] -> []
  | k, x :: xs when k = 0 -> xs
  | k, x :: xs -> [x] @ izbrisi (k-1) xs


let rec slice i k list =
  let rec do_elementa n = function
  | [] -> []
  | x :: xs -> if n = 0 then [] else x :: do_elementa (n-1) xs
  in
  let rec od_elementa n = function
  | [] -> []
  | x :: xs -> if n = 0 then (x::xs) else od_elementa (n-1) xs
  in
  do_elementa (k-i) (od_elementa i list);;

let rec vstavi x k list =
  match x, k, list with
  | x, k, [] -> [x] @ []
  | x, k, list when k <= 1 -> [x] @ list
  | x, k, list when k = List.length list -> list @ [x]
  | x, k, y :: ys -> [y] @ vstavi x (k-1) ys


  let rec rotiraj n list =
  match n, list with
  | n, [] -> []
  | n, list when n = 0 -> list
  | n, x :: xs -> rotiraj (n-1) xs @ [x]


let rec odstrani x list = 
  match x, list with
  | x, [] -> []
  | x, y :: ys when y = x -> odstrani x ys
  | x, y :: ys -> [y] @ odstrani x ys

  
let rev list =
  let rec aux acc = function
    | [] -> acc
    | h::t -> aux (h::acc) t in
  aux [] list;;


let rec palindrom = function
  | list when list = rev list -> true
  | list -> false


let rec maks_komponente list1 list2 =
  match list1, list2 with
  | [], [] -> []
  | xs, [] -> []
  | [], ys -> []
  | x :: xs, y :: ys when x > y -> [x] @ maks_komponente xs ys
  | x :: xs, y :: ys when y > x -> [y] @ maks_komponente xs ys
  | list1, list2 -> list1


let rec druga_najvecja list =
  let rec najvecja = function
    | [] -> failwith "prekratek seznam"
    | x :: [] -> x
    | x :: xs -> max x (najvecja xs)
  in
najvecja (odstrani (najvecja list) list)

(*uvod v ocaml*)