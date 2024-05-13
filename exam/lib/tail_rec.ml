(* Результат работы функции либо число, либо вызов функции *)
(* Советы: создать доп. функцию с доп. аргументом. В базовом случае вспомогательная функция просто возвращает аккумулятор. *)
let fac n = let rec helper acc n = if (n <= 1) then acc else helper (acc * n) (n - 1) in helper 1 n;;

let map (t : 'a -> 'b) (list: 'a list): 'b list = 
  let rec helper (acc: 'b list) (t : 'a -> 'b) = function 
  | [] -> acc
  | hd :: tl -> helper (acc @ [t hd]) t tl in helper [] t list;;

let filter (filt: 'a -> bool) (list: 'a list): 'a list = 
  let rec helper (acc: 'a list) (filt: 'a -> bool) (list: 'a list): 'a list =
    match list with 
    | [] -> acc
    | hd :: tl -> helper (acc @ (if filt hd then [hd] else [])) filt tl in
    helper [] filt list;;

(* свёртка - преобразование структуры данных к единственному атомарному значению *)
let fold_left (f: 'a -> 'b -> 'a) (init: 'a) (list: 'b list): 'a =
  let rec helper (acc: 'a) (f: 'a -> 'b -> 'a) (list: 'b list): 'a =
    match list with
    | [] -> acc
    | hd :: tl -> helper (f acc hd) f tl in
    helper init f list;;

    (* acc = f a_i acc*)
    (* non-tail recursive*)
let rec fold_right (f: 'a -> 'acc -> 'acc) (init: 'acc) = function
  | [] -> init
  | hd::[] -> f hd init
  | hd::tl -> f hd (fold_right f init tl)
