module type Queue = sig

  type 'a t
  val empty : 'a t (*Создание пустой очереди*)
  val is_empty : 'a t -> bool (* проверка на пустоту *)
  val put : 'a t -> 'a -> 'a t (*добавление в очередь*)
  val pop : 'a t -> 'a t (* удаление главного элемента *)
  val head : 'a t -> 'a option (*просмотр головного элемента за O(1)*)
end

module BatchedQueue : Queue with type 'a t = 'a list * 'a list  = struct
  type 'a t = 'a list * 'a list

  let empty = [], [];;

  let is_empty = function
  | [], [] -> true
  | _, _ -> false;;

  let put ((f, e): 'a list * 'a list) (x: 'a) = match (f, e) with
  | [], [] ->  ([x], [])
  | [], _ -> (List.rev ([x] @ e), [])
  | _, _ -> (f, [x] @ e);;

  let head = function
  | [], _ -> None
  | x :: _, _ -> Some x;;

  let rec pop (f, e) = match (f, e) with
  | [], [] -> [], []
  | [], e -> pop (List.rev e, [])
  | [_], e -> (List.rev e, [])
  | _ :: f, _ -> (f, e);; 

end

module Test1 = struct
  open BatchedQueue
  let queue = put (put (put empty 1) 2) 3

  let%test _ = head queue = Some 1
  let%test _ = head (pop queue) = Some 2
  let%test _ = head (pop (pop queue)) = Some 3
end
