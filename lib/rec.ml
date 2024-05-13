(* катаморфизм *)
let fold_left f init xs =
  let rec helper f acc xs =
    match xs with
    | [] -> acc
    | h :: t -> helper f (f acc h) t in
    helper f init xs;;
  
let rec fold_right f init xs =
  match xs with
  | [] -> init
  | x :: xs -> f x (fold_right f init xs);;

let rec insert x = function
 | [] -> [x]
 | h :: tl when h < x -> x :: h :: tl 
 | h :: tl -> h :: insert x tl;;

 (* анаморфизм *)

let rec ana fin next get x = if not (fin x) then [] else get x :: ana fin next get (next x);;  

let cata = fold_right;;

let hylo f acc fin get next x = cata f acc (ana fin get next x);;

let fac = hylo ( * ) 1 (fun x -> x = 0) Fun.id (fun x -> x - 1);;