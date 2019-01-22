type kompleksno = {re: float; im: float}
type polarno = {kot: float; radij: float}

(*let i = {re = 0.0; im = 1.0}  -> vrne val i : kompleksno = {re = 0.0; im = 1.0}. let abs z = sqrt (z.re ** 2. +. z.im ** 2.);;*)

type euro = Euro of float
type dollar = Dollar of float

let dollar_to_euro (Dollar x) = Euro (x *. 0.861) (*dollar_to_euro (Dollar 0.5);;*)
let euro_to_dollar (Euro x) = Dollar (x *. 1.161)


 type currency =
  | Jen of float
  | Funt of float
  | Krona of float

let to_pound = function
  | Jen x -> Funt (x *. 0.00707)
  | Funt x -> Funt x
  | Krona x -> Funt (x *. 0.0859) 


type intbool_list = 
  | Int of int * intbool_list
  | Bool of bool * intbool_list
  | Nil
  
let test = Int(5, Bool(true, Bool(false, Int(7, Nil))))


let rec intbool_map f_int f_bool ib_list =
  match ib_list with
  | Int(x, xs) -> Int(f_int x, intbool_map f_int f_bool xs)
  | Bool(x, xs) -> Bool(f_bool x, intbool_map f_int f_bool xs)
  | Nil -> Nil



let rec intbool_reverse intbool_list = 
  let rec intbool_reverse' acc = function
  | Nil -> acc
  | Bool(x,xs) -> intbool_reverse' (Bool(x, acc)) xs
  | Int(x, xs) -> intbool_reverse' (Int(x, acc)) xs
  in
intbool_reverse' Nil intbool_list


let rec intbool_seperate ib_list = 
  let rec intbool_seperate' acc1 acc2 = function
  | Nil -> (acc1, acc2)
  | Int(x, xs) -> intbool_seperate' (Int(x, acc1)) acc2 xs
  | Bool(x, xs) -> intbool_seperate' acc1 (Bool(x, acc2)) xs
  in
intbool_seperate' Nil Nil ib_list




type magic = 
  | Fire
  | Frost
  | Arcane

type specialisation = 
  | Historian
  | Teacher
  | Researcher


type status = 
  | Newbie
  | Student of magic * int
  | Employed of magic * specialisation

type wizard = {name: string; status: status}


let ucenec = {name = "Ziga"; status =  Student (Fire, 8)} 
let professor = {name = "Matija"; status = Employed(Fire, Teacher)}


type magic_counter = {fire: int; frost: int; arcane: int}

let update counter = function
  | Frost -> {counter with frost = counter.frost + 1}
  | Fire -> {counter with fire = counter.fire + 1}
  | Arcane -> {counter with arcane = counter.arcane + 1}


let count_magic seznam = 
  let rec counter count = function
  | [] -> count
  | {name; status} :: ostali -> (
      match status with
      | Newbie -> counter count ostali
      | Student (tip, _) -> counter (update count tip) ostali
      | Employed (tip, _) -> counter (update count tip) ostali
  )
  in
counter {fire = 0; frost = 0; arcane = 0} seznam


let find_candidate magic specialisation wizard_list = 
  let leto = 
    match specialisation with
    | Historian -> 3
    | Teacher -> 4
    | Researcher -> 5
  in
  let rec search = function
  | [] -> None
  | {name; status} :: wizards ->
      match status with
      | Student (m, y) when m = magic && y >= leto -> Some name
      | _ -> search wizards
in
search wizard_list
