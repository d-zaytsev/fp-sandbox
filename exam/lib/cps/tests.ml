let tail_map f xs = 
  let rec helper f xs acc = match xs with
  | [] -> acc
  | x :: xs -> helper f xs (acc @ [f x]) in
  helper f xs [];;
  
let cps_map f xs =
  let rec helper f xs r = match xs with
  | [] -> r []
  | h :: tl -> helper f tl (fun x -> r ([f h] @ x)) in
  helper f xs Fun.id;;


(* tail rec *)
let fold_left xs init f =
  let rec helper xs acc f = 
  match xs with
  | [] -> acc
  | hd :: tl -> helper tl (f acc hd) f in
  helper xs init f;;

(* cps *)

let fold_right xs init (f: 'a -> 'b -> 'b) =
  let rec helper xs r init f =
    match xs with
    | [] -> r init
    | hd :: tl -> helper tl (fun x -> r (f hd x)) init f in
  helper xs Fun.id init f;;