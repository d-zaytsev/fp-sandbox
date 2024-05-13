(* 
  works with -rectypes
let z =  fun f-> let g = fun x -> f(fun v-> x x v)in g g;;
let fac = z (fun f n -> if n = 1 then 1 else n * f (n-1));; 
*)