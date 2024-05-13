open Angstrom

let parsik = take_while (fun c -> c == 'a') >>= 
fun an -> take_while (fun c -> c == 'b') >>= 
fun bn -> take_while (fun c -> c == 'c') >>=
fun cn ->  if ((String.length an) == (String.length bn) && (String.length an) == (String.length cn)) then return true
else fail "Error"

let eval (str:string) =
  match parse_string ~consume:All parsik str with
  | Ok v      -> v
  | Error msg -> failwith msg;;