(* ========== Vaje 6: Dinamično programiranje  ========== *)


(*----------------------------------------------------------------------------*]
 Požrešna miška se nahaja v zgornjem levem kotu šahovnice. Premikati se sme
 samo za eno polje navzdol ali za eno polje na desno in na koncu mora prispeti
 v desni spodnji kot. Na vsakem polju šahovnice je en sirček. Ti sirčki imajo
 različne (ne-negativne) mase. Miška bi se rada kar se da nažrla, zato jo
 zanima, katero pot naj ubere.

 Funkcija [max_cheese cheese_matrix], ki dobi matriko [cheese_matrix] z masami
 sirčkov in vrne največjo skupno maso, ki jo bo miška požrla, če gre po
 optimalni poti.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # max_cheese test_matrix;;
 - : int = 13
[*----------------------------------------------------------------------------*)
let test_matrix = 
  [| [| 1 ; 2 ; 0 |];
     [| 2 ; 4 ; 5 |];
     [| 7 ; 0 ; 1 |] |]

let max_cheese cheese_matrix =
    let max_r = Array.length cheese_matrix in
    let max_c = Array.length cheese_matrix.(0) in

    let rec max_cheese' r c =
    if r >= max_r || c >= max_c then
     0
    else
      let right = max_cheese' r (c+1) in
      let down = max_cheese' (r+1) c in 
      let our_cheese = cheese_matrix.(r).(c) in
      our_cheese + max right down
in
max_cheese' 0 0


(*----------------------------------------------------------------------------*]
 Rešujemo problem sestavljanja alternirajoče obarvanih stolpov. Imamo štiri
 različne tipe gradnikov, dva modra in dva rdeča. Modri gradniki so višin 2 in
 3, rdeči pa višin 1 in 2.

 Funkcija [alternating_towers] za podano višino vrne število različnih stolpov
 dane višine, ki jih lahko zgradimo z našimi gradniki, kjer se barva gradnikov
 v stolpu izmenjuje (rdeč na modrem, moder na rdečem itd.). Začnemo z gradnikom
 poljubne barve.

 Namig: Uporabi medsebojno rekurzivni pomožni funkciji z ukazom [and].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # alternating_towers 10;;
 - : int = 35
[*----------------------------------------------------------------------------*)
let articles = [|
	("yoghurt", 0.39, 0.18);
	("milk", 0.89, 1.03);
  ("coffee", 2.19, 0.2);
  ("butter", 1.49, 0.25);
  ("yeast", 0.22, 0.042);
  ("eggs", 2.39, 0.69);
  ("sausage", 3.76, 0.50);
  ("bread", 2.99, 1.0);
  ("Nutella", 4.99, 0.75);
  ("juice", 1.15, 2.0)
|]

let best_value articles max_w =
  let rec get_item acc_w acc_p (_, p, w) =
    if acc_w +. w > max_w then
      acc_p
    else
      shopper (acc_w +. w) (acc_p +. p)
  and shopper w p =
    let choices = Array.map (get_item w p) articles in
    Array.fold_left max 0. choices
  in
  shopper 0. 0.

let best_value_unique articles max_w =
  (* Store which items have already been chose in the array [taken]. *)
  let rec get_item taken acc_w acc_p i (_, p, w) =
    if acc_w +. w > max_w || taken.(i) then
      acc_p
    else
      let new_taken = Array.copy taken in
      (new_taken.(i) <- true; shopper new_taken (acc_w +. w) (acc_p +. p))
  and shopper taken w p =
    let choices = Array.mapi (get_item taken w p) articles in
    Array.fold_left max 0. choices
  in
  let taken = Array.map (fun _ -> false) articles in
shopper taken 0. 0.