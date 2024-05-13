module type Monad = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

(* простая монада для тестов *)
module Option_Monad : Monad with type 'a t = 'a option =
struct
  type 'a t = 'a option

  let return x = Some x
  
  let ( >>= ) m f = match m with
  | Some v -> f v 
  | None -> None;;
end

module type Applicative = sig
  type 'a t

  val pure: 'a -> 'a t
  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
end

module type Functor = sig
  type 'a t

  val pure: 'a -> 'a t
  val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
end

(* из монады можно сделать аппликатив
   передаём в этот модуль монаду на которой он строится, т.е. передаём монаду получаем аппликатив *)
module ApplicativeOfMonad (M: Monad) : Applicative with type 'a t = 'a M.t = struct
  include M

  type 'a t = 'a M.t
  let pure x = return x
  let ( <*> ) f x = f >>= fun f -> x >>= fun x -> return (f x)
end

module FunctorOfApplicative (A: Applicative) : Functor with type 'a t = 'a A.t = struct
  include A

  type 'a t = 'a A.t

  let pure x = pure x

  let ( <$> ) (f: 'a -> 'b) (x: 'a t) : 'b t = pure f <*> x
end

module Test1 = struct
  open ApplicativeOfMonad(Option_Monad)

  let res smth r = match smth with
  | Some v when v = r -> true
  | _ -> false;;

  let%test _ = res (pure 123) 123

  let%test _ = res (pure (-) <*> (pure (+) <*> pure 1 <*> pure 2) <*> pure 0) 3
end

module Test2 = struct
  (* функтор из аппликатива из монады *)
  open FunctorOfApplicative(ApplicativeOfMonad(Option_Monad))

  let res smth r = match smth with
  | Some v when v = r -> true
  | _ -> false;;

  let%test _ = res (pure 123) 123

  let%test _ = res ((fun x -> x + 2) <$> pure 1) 3
end