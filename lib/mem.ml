open Base

(* мемоизация функцию без рекурсии *)
let mem (f: 'a -> 'b) x = 
  let table = Hashtbl.Poly.create () in
  match Hashtbl.find table x with
  | Some res -> res
  | None -> Hashtbl.add_exn table ~key:x ~data:(f x); f x;;

let rec fac n = if (n <= 1) then 1 else n * fac(n - 1);;
let mfac = mem fac;;

(* -- открытая рекурсия -- *)
 let open_mem f x =
  let table = Hashtbl.Poly.create () in
  let rec helper x = match Hashtbl.find table x with
  | Some res -> res
  | None -> let res = f helper x in Hashtbl.add_exn table ~key:x ~data:(res); res
   in
  helper x;;

let open_fib self = function
 | 0 -> 0
 | 1 -> 1
 | n -> self(n - 2) + self(n - 1);;

 let fib n = open_mem open_fib n;;

let open_fac self = function
  | 1 -> 1
  | n -> n * self (n-1);;

  let fac n = open_mem open_fac n;;

(* тесты *)

  let%test _ = fac 5 = 120;;
  let%test _ = fib 5 = 5;;