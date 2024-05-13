module type Monad = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

module Option_Monad : Monad with type 'a t = 'a option =
struct
  type 'a t = 'a option

  let return x = Some x
  
  let ( >>= ) m f = match m with
  | Some v -> f v 
  | None -> None;;
end

module Result_Monad : sig
  include Monad with type 'a t = ('a, string) result 

  val fail : string -> ('a, string) result

 end =
struct
  type 'a t = ('a, string) result

  let return x = Ok x

  let fail str : ('a, string) result = Error str

  let ( >>= ) m f : ('b, string) result = 
  match m with
  | Ok v -> f v
  | Error x -> Error x;;
end

module Identify_Monad : Monad with type 'a t = 'a = struct
    type 'a t = 'a

  let return x = x

  let ( >>= ) m f : 'a = f m
end

module List_Monad : Monad with type 'a t = 'a list = struct
    type 'a t = 'a list

  let return x = [x]

  let ( >>= ) m f = List.concat (List.map f m);;
end

module Parser_Monad : sig
type input = char list
  type 'a parse_result = 
  | Failed
  | Parsed of 'a * input

include Monad with type 'a t = input -> 'a parse_result

end = struct
  type input = char list

  type 'a parse_result = 
  | Failed
  | Parsed of 'a * input

  type 'a t = input -> 'a parse_result

  let return x : 'a t = fun stream -> Parsed(x, stream) 

  let ( >>= ) (m : 'a t) (f : 'a -> 'b t) : 'b t = 
  fun stream -> 
	   match m stream with (* парсим stream *)
		   | Failed -> Failed 
		   | Parsed (h, tl) -> (f h) tl

end

module Concurrent_Monad : Monad with type 'a t = 'a Lwt.t = struct

  type 'a t = 'a Lwt.t
  let return = Lwt.return
  let ( >>= ) = Lwt.bind
end

module Test1 = struct
 open Result_Monad

  let fac n = 
    let rec helper acc n = match n with
    | 1 -> acc
    | n -> helper (acc * n) (n-1) in
    return (helper 1 n);; 

  let res smth r = match smth with
  | Ok v when v = r -> true
  | _ -> false;;

  let%test _ = res (fac 1 >>= fun x -> fac (x + 1)) 2;;
  
end

module Test2 = struct
 open List_Monad

  let res smth r = match smth with
  | v when v = r -> true
  | _ -> false;;

  let%test _ = res ([1; 2; 3; 4] >>= fun x -> [x + 1]) [2; 3; 4; 5];;
  
end