type 'a tree =
  | Empty
  | Node of 'a tree * 'a * 'a tree

let leaf x = Node(Empty, x, Empty)

(*----------------------------------------------------------------------------*]
 Definirajmo si testni primer za preizkušanje funkcij v nadaljevanju. Testni
 primer predstavlja spodaj narisano drevo, pomagamo pa si s pomožno funkcijo
 [leaf], ki iz podatka zgradi list.
          5
         / \
        2   7
       /   / \
      0   6   11
[*----------------------------------------------------------------------------*)

let test_tree = 
  let left_t = Node(leaf 0, 2, Empty) in
  let right_t = Node (leaf 6, 7, leaf 11) in
  Node(left_t, 5, right_t)

  
let rec mirror = function
  | Empty -> Empty
  | Node(l, x, d) ->
  let novi_l = mirror d in
  let novi_d = mirror l in 
  Node(novi_l, x, novi_d)


let rec height = function
  | Empty -> 0
  | Node(l, x, d) -> 1 + max (height l) (height d)

let rec size = function
  | Empty -> 0
  | Node(l, x, d) -> 1 + (size l) + (size d)


let rec map_tree f = function
  | Empty -> Empty
  | Node(l, x, d) -> Node((map_tree f l), (f x), (map_tree f d))


let rec list_of_tree = function
  | Empty -> []
  | Node(l, x, r) -> (list_of_tree l) @ [x] @ (list_of_tree r)


let rec is_bst tree = 
  let rec narascajoce_zaporedje = function
    | [] -> true
    | x :: [] -> true
    | x :: (y :: xs as ostanek) when x < y -> narascajoce_zaporedje ostanek
    | list -> false
  in 
narascajoce_zaporedje (list_of_tree tree)


(*Sedaj so vsa drevesa BST*)

let rec insert x = function
  | Empty -> Node(Empty, x, Empty)
  | Node(l, y, r) when x < y -> Node((insert x l), y, r)
  | Node(l, y, r) when x = y -> failwith "Ze v seznamu!"
  | Node(l, y, r) -> Node(l, y, (insert x r))

let rec member x = function
  | Empty -> false
  | Node(l, y, r) when x < y -> member x l
  | Node(l, y, r) when x = y -> true
  | Node(l, y, r) -> member x r

let rec member2 k = function
  | Empty -> false
  | Node(left, x, right) -> if k = x then true
  else member2 k right or member2 k left


let succ bst =
  let rec minimal = function
    | Empty -> None
    | Node (Empty, x, _) -> Some x
    | Node (l, _, _) -> minimal l
  in
  match bst with
  | Empty -> None
  | Node (_, _, r) -> minimal r

let pred bst =
  let rec maximal = function
    | Empty -> None
    | Node (_, x, Empty) -> Some x
    | Node (_, _, r) -> maximal r
  in
  match bst with
  | Empty -> None
  | Node (l, _, _) -> maximal l


let rec delete x = function
| Empty -> Empty
| Node (l, y, r) when x > y -> Node (l, y, delete x r)
| Node (l, y, r) when x < y -> Node (delete x l, y, r)
| Node (l, y, r) as bst -> (
    (*We need to delete the root.*)
    match succ bst with
    | None -> l (*Only happens when [r] is [Empty].*)
    | Some s ->
      let clean_r = delete s r in
      Node (l, s, clean_r))





(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 SLOVARJI

 S pomočjo BST lahko (zadovoljivo) učinkovito definiramo slovarje. V praksi se
 slovarje definira s pomočjo hash tabel, ki so še učinkovitejše. V nadaljevanju
 pa predpostavimo, da so naši slovarji [dict] binarna iskalna drevesa, ki v
 vsakem vozlišču hranijo tako ključ kot tudi pripadajočo vrednost, in imajo BST
 strukturo glede na ključe. Ker slovar potrebuje parameter za tip ključa in tip
 vrednosti, ga parametriziramo kot [('key, 'value) dict].
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)


type ('key, 'value) dict = ('key * 'value) tree

(*----------------------------------------------------------------------------*]
 Napišite testni primer [test_dict]:
      "b":1
      /    \
  "a":0  "d":2
         /
     "c":-2
[*----------------------------------------------------------------------------*)

let test_dict
  : (string, int) dict
  = Node(leaf ("a", 0), ("b", 1), Node(leaf ("c", -2), ("d", 2), Empty))

(*----------------------------------------------------------------------------*]
 Funkcija [dict_get key dict] v slovarju poišče vrednost z ključem [key]. Ker
 slovar vrednosti morda ne vsebuje, vrne [option] tip.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dict_get "banana" test_dict;;
 - : 'a option = None
 # dict_get "c" test_dict;;
 - : int option = Some (-2)
[*----------------------------------------------------------------------------*)

let rec dict_get key = function
  | Empty -> None
  | Node(l, (k, v), r) ->
  if key = k then 
    Some v
  else if key < k then 
    dict_get key l
  else 
  dict_get key r
(*----------------------------------------------------------------------------*]
 Funkcija [print_dict] sprejme slovar s ključi tipa [string] in vrednostmi tipa
 [int] in v pravilnem vrstnem redu izpiše vrstice "ključ : vrednost" za vsa
 vozlišča slovarja.
 Namig: Uporabite funkciji [print_string] in [print_int]. Nize združujemo z
 operatorjem [^]. V tipu funkcije si oglejte, kako uporaba teh funkcij določi
 parametra za tip ključev in vrednosti v primerjavi s tipom [dict_get].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # print_dict test_dict;;
 a : 0
 b : 1
 c : -2
 d : 2
 - : unit = ()
[*----------------------------------------------------------------------------*)
let rec print_dict = function
  | Empty -> ()
  | Node (d_l, (k, v), d_r) -> (
      print_dict d_l;
      print_string (k ^ " : "); print_int v; print_newline ();
      print_dict d_r)


(*----------------------------------------------------------------------------*]
 Funkcija [dict_insert key value dict] v slovar [dict] pod ključ [key] vstavi
 vrednost [value]. Če za nek ključ vrednost že obstaja, jo zamenja.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dict_insert "1" 14 test_dict |> print_dict;;
 1 : 14
 a : 0
 b : 1
 c : -2
 d : 2
 - : unit = ()
 # dict_insert "c" 14 test_dict |> print_dict;;
 a : 0
 b : 1
 c : 14
 d : 2
 - : unit = ()
[*----------------------------------------------------------------------------*)

let rec dict_insert k v = function
  | Empty -> leaf (k, v)
  | Node (l, (k', _), r) when k = k' -> Node (l, (k, v), r)
  | Node (l, (k', v'), r) when k < k' -> Node (dict_insert k v l, (k', v'), r)
| Node (l, (k', v'), r) (* when k > k' *) -> Node (l, (k', v'), dict_insert k v r)