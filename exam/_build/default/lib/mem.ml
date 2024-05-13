open Base

let mem (f: 'a -> 'b) x = 
  let table = Hashtbl.Poly.create () in
  match Hashtbl.find table x with
  | Some res -> res
  | None -> Hashtbl.add_exn table ~key:x ~data:(f x); f x;;

let rec fac n = if (n <= 1) then 1 else n * fac(n - 1);;
let mfac = mem fac;;

(* open rec *)
let open_fib self = function
 | 0 -> 0
 | 1 -> 1
 | n -> self(n - 2) + self(n - 1);;

 let rec fib n = open_fib fib n;;

 let open_mem f x =
  let table = Hashtbl.Poly.create () in
  let rec helper x = match Hashtbl.find table x with
  | Some res -> res
  | None -> Hashtbl.add_exn table ~key:x ~data:(f helper x); f helper x in
 helper x;;