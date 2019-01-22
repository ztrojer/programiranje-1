let rec zlozi_desno f xs z = match xs with
  | [] -> z
  | x :: xs -> f x (zlozi_desno f xs z)

let vsota xs =  zlozi_desno (+) xs 0

let produkt xs = zlozi_desno ( * ) xs 1

let dolzina xs = zlozi_desno (fun _ acc -> succ acc) xs 0

let preslikaj f xs = zlozi_desno (fun x acc -> f x :: acc) xs []

(*obračanje seznama - repno rekurzivno*)

let rec obrni list = 
  let rec obrni' acc = function
    | [] -> acc
    | x :: xs -> obrni' (x :: acc) xs 
  in
obrni' [] list

(*ponavljanje elementa*)

let rec ponavljaj x n = 
  let rec ponavljaj' x n acc =
    if n <= 0 then acc
    else 
      let new_acc = x :: acc in
    ponavljaj' x (n-1) new_acc
  in
ponavljaj' x n []

(*seznam stevil range(5)*)

let rec range n = 
  let rec range' n acc =
    if  n <= 0 then
      0 :: acc
    else
      range' (n-1) (n :: acc)
  in
range' n []
  
(*izvede preslikavo na seznamu*)

let rec map f list = 
  let rec map' f acc = function 
  | [] -> acc
  | x :: xs -> map' f (f x :: acc) xs
  in
map' f [] (obrni list)


let rec mapi f list = 
  let rec mapi' f acc indeks = function 
    | [] -> acc
    | x :: xs -> mapi' f (f indeks x :: acc) (indeks - 1) xs
  in
mapi' f [] (List.length list -1) (obrni list)


let rec zip list1 list2 = 
  let rec zip' acc list1 list2 =
    if List.length list1 != List.length list2 then failwith "Razlicna dolzina seznamov"
    else
      match list1, list2 with
      | [], [] -> acc
      | [], list2 -> failwith "Razlicna dolzina seznamov"
      | list1, [] -> failwith "Razlicna dolzina seznamov"
      | x :: xs, y :: ys -> zip' ((x, y) :: acc) xs ys
  in
zip' [] (obrni list1) (obrni list2) 


let rec zip_enum_tlrec list1 list2 = 
  let rec zip_enum_tlrec' acc i list1 list2 =
    if List.length list1 != List.length list2 then failwith "Razlicna dolzina seznamov"
    else
      match list1, list2 with
      | [], [] -> acc
      | [], list2 -> failwith "Negre"
      | list1, [] -> failwith "Tudi negre"
      | x :: xs, y :: ys -> zip_enum_tlrec' ((i, x, y) :: acc) (i-1) xs ys
  in
zip_enum_tlrec' [] (List.length list1 - 1) (obrni list1) (obrni list2)


let rec unzip seznam_parov = 
  let rec unzip' acc1 acc2 = function
  | [] -> (obrni acc1,obrni acc2)
  | (x1, x2) :: xs -> unzip' (x1 :: acc1) (x2 :: acc2) xs
  in
unzip' [] [] seznam_parov


let rec fold_left f list = 
  match list with
    | [] -> failwith "Prekratek seznam!"
    | x :: [] -> failwith "Prekratek seznam!"
    | x :: xs -> 
    let rec aux acc = function
    | [] -> acc
    | y :: ys -> aux (f acc y) ys
    in
aux x xs


let rec apply_sequence f x n = 
  match n with
    | n when n <= 0 -> failwith "premajhen n"
    | n -> 
    let rec apply_sequence' f x acc = function
      | n when n <= 0 -> List.rev acc
      | n -> apply_sequence' f (f x) (f x :: acc) (n-1)
    in
apply_sequence' f x [x] n


let rec filter f list = 
  let rec filter' f acc = function
  | [] -> List.rev acc
  | x :: xs when f x = true -> filter' f (x :: acc) xs
  | x :: xs -> filter' f acc xs
  in 
filter' f [] list


let rec exists f = function
  | [] -> false
| x :: xs -> if f x then true else exists f xs


let rec first f default list = 
  match list with
  | [] -> default
  | x :: xs -> if f x then x else first f default xs