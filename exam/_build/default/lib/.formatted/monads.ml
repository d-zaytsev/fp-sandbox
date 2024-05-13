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

module Result_Monad : Monad with              type 'a t = ('a, string) result =
struct

  type 'a t = ('a, string) result

  let return x = Ok x

  let fail str : ('a, string) result = Error str

  let ( >>= ) m f : ('b, string) result = 
  match m with
  | Ok v -> f v
  | Error x -> Error x;;
end

module Test1 = struct
  open Result_Monad

  let fac n = 
    let rec helper acc n = helper (acc * n) (n-1) in
    return (helper 1 n);; 

  let res smth r = match smth with
  | Ok v when v = r -> true
  | _ -> false;;

  let%test _ = res (return 125) 125;;
  
end
