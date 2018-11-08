
(* ========== Vaja 1: Uvod v OCaml  ========== *)

(*----------------------------------------------------------------------------*]
 Funkcija [penultimate_element] vrne predzadnji element danega seznama. V
 primeru prekratkega seznama vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # penultimate_element [1; 2; 3; 4];;
 - : int = 3
[*----------------------------------------------------------------------------*)

(* Funkcija za zadnji element*)
let rec ultimate_element list =
  match list with
  | [] -> failwith "list too short."
  | y :: [] -> y
  | y :: ys -> ultimate_element (ys)

let rec penultimate_element = function
  | [] -> failwith "list too short."
  | x :: [] -> failwith "list too short."
  | x :: _ :: [] -> x
  | _ :: y :: ys -> penultimate_element (y :: ys)

(*----------------------------------------------------------------------------*]
 Funkcija [get k list] poišče [k]-ti element v seznamu [list]. Številčenje
 elementov seznama (kot ponavadi) pričnemo z 0. Če je k negativen, funkcija
 vrne ničti element. V primeru prekratkega seznama funkcija vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # get 2 [0; 0; 1; 0; 0; 0];;
 - : int = 1
[*----------------------------------------------------------------------------*)
  let rec get k = function
    | [] -> failwith "List too short!"
    | x :: xs when k <= 0-> x
    | x :: xs -> get (k-1) xs

(*----------------------------------------------------------------------------*]
 Funkcija [double] podvoji pojavitve elementov v seznamu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # podvoji [1; 2; 3];;
 - : int list = [1; 1; 2; 2; 3; 3]
[*----------------------------------------------------------------------------*)

let rec podvoji = function
    | [] -> []
    | x :: [] -> [x ; x]
    | x :: xs -> [x; x] @ podvoji xs  

   (* x :: [x]
    x::x::[]
    [x;x] *)

(*----------------------------------------------------------------------------*]
 Funkcija [divide k list] seznam razdeli na dva seznama. Prvi vsebuje prvih [k]
 elementov, drugi pa vse ostale. Funkcija vrne par teh seznamov. V primeru, ko
 je [k] izven mej seznama, je primeren od seznamov prazen.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # divide 2 [1; 2; 3; 4; 5];;
 - : int list * int list = ([1; 2], [3; 4; 5])
 # divide 7 [1; 2; 3; 4; 5];;
 - : int list * int list = ([1; 2; 3; 4; 5], [])
[*----------------------------------------------------------------------------*)

let rec divide k list =
  match k, list with
  | k, list when k <= 0 -> ([], list)
  | k, [] -> ([], [])
  | k, x :: xs ->
    let (left, right) = divide (k-1) xs in
    (x :: left, right)

(*----------------------------------------------------------------------------*]
 Funkcija [delete k list] iz seznama izbriše [k]-ti element. V primeru
 prekratkega seznama funkcija vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # delete 3 [0; 0; 0; 1; 0; 0];;
 - : int list = [0; 0; 0; 0; 0]
[*----------------------------------------------------------------------------*)

let rec delete k list =
  match k, list with
  | k, [] -> []
  | k, x :: xs when k = 0 -> xs
  | k, x :: xs -> [x] @ delete (k-1) xs

(*----------------------------------------------------------------------------*]
 Funkcija [slice i k list] sestavi nov seznam, ki vsebuje elemente seznama
 [list] od vključno [i]-tega do izključno [k]-tega. Predpostavimo, da sta [i] in
 [k] primerna.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # slice 3 6 [0; 0; 0; 1; 2; 3; 0; 0];;
 - : int list = [1; 2; 3]
[*----------------------------------------------------------------------------*)

let rec slice i k list =
  let rec do_elementa n = function
  | [] -> []
  | x :: xs -> if n = 0 then [] else x :: do_elementa (n-1) xs
  in
  let rec od_elementa n = function
  | [] -> []
  | x :: xs as sez -> if n = 0 then sez else od_elementa (n-1) xs
  in
  do_elementa (k-i) (od_elementa i list);;
(*----------------------------------------------------------------------------*]
 Funkcija [insert x k list] na [k]-to mesto seznama [list] vrine element [x].
 Če je [k] izven mej seznama, ga funkcija doda na začetek oziroma na konec.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # insert 1 3 [0; 0; 0; 0; 0];;
 - : int list = [0; 0; 0; 1; 0; 0]
 # insert 1 (-2) [0; 0; 0; 0; 0];;
 - : int list = [1; 0; 0; 0; 0; 0]
[*----------------------------------------------------------------------------*)

let rec insert x k list =
  match x, k, list with
  | x, k, [] -> [x] @ []
  | x, k, list when k <= 1 -> [x] @ list
  | x, k, list when k = List.length list -> list @ [x]
  | x, k, y :: ys -> [y] @ insert x (k-1) ys

(*----------------------------------------------------------------------------*]
 Funkcija [rotate n list] seznam zavrti za [n] mest v levo. Predpostavimo, da
 je [n] v mejah seznama.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # rotate 2 [1; 2; 3; 4; 5];;
 - : int list = [3; 4; 5; 1; 2]
[*----------------------------------------------------------------------------*)

let rec rotate n list =
  match n, list with
  | n, [] -> []
  | n, list when n = 0 -> list
  | n, x :: xs -> rotate (n-1) xs @ [x]


(*----------------------------------------------------------------------------*]
 Funkcija [remove x list] iz seznama izbriše vse pojavitve elementa [x].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # remove 1 [1; 1; 2; 3; 1; 2; 3; 1; 1];;
 - : int list = [2; 3; 2; 3]
[*----------------------------------------------------------------------------*)

let rec remove x list = 
  match x, list with
  | x, [] -> []
  | x, y :: ys when y = x -> remove x ys
  | x, y :: ys -> [y] @ remove x ys
(*----------------------------------------------------------------------------*]
 Funkcija [is_palindrome] za dani seznam ugotovi ali predstavlja palindrom.
 Namig: Pomagaj si s pomožno funkcijo, ki obrne vrstni red elementov seznama.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # is_palindrome [1; 2; 3; 2; 1];;
 - : bool = true
 # is_palindrome [0; 0; 1; 0];;
 - : bool = false
[*----------------------------------------------------------------------------*)

let rev list =
  let rec aux acc = function
    | [] -> acc
    | h::t -> aux (h::acc) t in
  aux [] list;;

let rec is_palindrome list =
  match list with
  | [] -> true
  | list when list = rev list -> true
  | list -> false
(*----------------------------------------------------------------------------*]
 Funkcija [max_on_components] sprejme dva seznama in vrne nov seznam, katerega
 elementi so večji od istoležnih elementov na danih seznamih. Skupni seznam ima
 dolžino krajšega od danih seznamov.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # max_on_components [5; 4; 3; 2; 1] [0; 1; 2; 3; 4; 5; 6];;
 - : int list = [5; 4; 3; 3; 4]
[*----------------------------------------------------------------------------*)

let rec max_on_components list1 list2 =
  match list1, list2 with
  | [], [] -> []
  | [], ys -> []
  | xs, [] -> []
  | x :: xs, y :: ys when x > y -> [x] @ max_on_components xs ys
  | x :: xs, y :: ys when y > x -> [y] @ max_on_components xs ys
  | list1, list2 -> list1

(*----------------------------------------------------------------------------*]
 Funkcija [second_largest] vrne drugo največjo vrednost v seznamu. Pri tem se
 ponovitve elementa štejejo kot ena vrednost. Predpostavimo, da ima seznam vsaj
 dve različni vrednosti.
 Namig: Pomagaj si s pomožno funkcijo, ki poišče največjo vrednost v seznamu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # second_largest [1; 10; 11; 11; 5; 4; 10];;
 - : int = 10
[*----------------------------------------------------------------------------*)

let rec second_largest list =
  let rec largest k = function
  | [] -> 0
  | x :: [] -> if k >= x then k else x
  | x :: xs -> if k >= x then largest k xs else largest x xs
  largest 0 (remove (largest 0 list) list)