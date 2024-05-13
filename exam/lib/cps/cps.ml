(* k - продолжение, что делать с результатом функции *)
let rec fac k n = if n <= 1 then k 1 else fac (fun x -> k (x * n)) (n - 1);;

let rec len res = function
| [] -> res 0
| _ :: tl -> len (fun x -> res (x + 1)) tl;;

let rec sum (res: int -> int) = function
| [] -> res 0
| hd :: tl -> sum (fun x -> res (x + hd)) tl;;

let rec fib (res : int -> int) = function
  | 0 -> res 0
  | 1 -> res 1
  | n -> fib (fun x -> fib (fun y -> res (x + y)) (n-2)) (n-1);;

let rec map (res: 'a list -> 'b list) (f : 'a -> 'b) (list: 'a list) : 'b list = match list with
| [] -> res []
| hd :: tl -> map (fun x -> res ([f hd] @ x)) f tl;;