open Angstrom

(* парсеры a^n b^n c^n *)
let parsik = take_while (fun c -> c == 'a') >>= 
fun an -> take_while (fun c -> c == 'b') >>= 
fun bn -> take_while (fun c -> c == 'c') >>=
fun cn ->  if ((String.length an) == (String.length bn) && (String.length an) == (String.length cn)) then return true
else fail "Error"

let parser = let rec repeat p n = 
  	    if n = 0 then return None else p >>= fun _ -> repeat p (n-1) in
  take_while (fun c -> c == 'a') >>= fun a -> repeat (char 'b') (String.length a) >>= fun _ -> repeat (char 'c') (String.length a) >>= fun _ ->
    return true

  (* p1 >>= p2 - запускает парсер p1, передаёт его результат p2 и возвращает его результат *)

let eval p (str:string) =
  match parse_string ~consume:All p str with
  | Ok v      -> v
  | Error msg -> failwith msg;;

module Test1 = struct
  let%test _ = eval parser "aaabbbccc";;

  (* выполнение 3х законов монад для парсеров *)

  (* left identity (return x >>= f ~ f x) *)
  let%test _ = let f = fun x -> return (x + 1) in
   eval (return 1 >>= f) "" = eval (f 1) ""

  (* right identity (m >>= return ~ m, если m в контексте) *)
  let%test _ = let m = return "value in context" in
   eval (m >>= return) "" = eval (m) ""

  (* assoc (f >>= g >>= h ~ f >>= fun x -> g x >>= h) *)
  let%test _ = let f = return 1 in
  let g = fun x -> return (x + 1) in
  let h = fun x -> return (x * 2) in
   eval (f >>= g >>= h) "" = eval (f >>= fun x -> g x >>= h) ""

end