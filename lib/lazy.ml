type 'a lazy_list = 
  | Nil
  | Cons of 'a * (unit -> 'a lazy_list);;
(* когда понадобится хвост будет вызвана функция и хвост вычислится *)

let rec zero_one_list = Cons(0, (fun _ -> Cons(1, (fun _ -> zero_one_list))));; 

let get = 
  let rec helper acc n = function
  | Nil -> -1
  | Cons (el, _) when acc == n -> el
  | Cons(_, f) -> helper (acc + 1) n (f ()) (* просто вызываем функцию чтобы перейти к следующему элементу *)
in
helper 0;;  

let rec fib = 
  let shift = function
  | Nil -> failwith "wtf"
  | Cons(_, f) -> f () in
  let rec sum xs ys = match xs, ys with
  | Nil, Nil -> Nil
  | _, Nil -> Nil
  | Nil, _ -> Nil
  | Cons(x, f1), Cons(y, f2) -> Cons(x + y, (fun () -> sum (f1 ()) (f2 ())))
    in
  Cons(0, fun () -> Cons(1, fun () -> sum fib (shift fib)));;