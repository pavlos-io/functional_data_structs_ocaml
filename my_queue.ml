module type MyQ = sig
  type 'a t

  val empty: 'a t
  val is_empty: 'a t -> bool
  val enq: 'a t -> 'a -> 'a t
  val peek: 'a t -> 'a option
  val deq: 'a t -> 'a t option
end

module Q: MyQ = struct
  type 'a t = 'a list

  let empty = []

  let is_empty q = List.length q = 0

  let enq q el = q @ [el]

  let peek = function
    | []      -> None
    | el :: _ -> Some(el)

  let deq = function
    | []      -> None
    | _ :: q' -> Some(q')
end