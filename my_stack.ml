module type MyStack = sig
  type 'a t
  
  val is_empty: 'a t -> bool
  val empty: 'a t
  val length: 'a t -> int
  val push: 'a -> 'a t -> 'a t
  val peek: 'a t -> 'a option
  val iter: 'a t -> f:('a -> unit) -> unit
end

module ListStack: MyStack = struct
  type 'a t = 'a list

  let is_empty t = List.length t = 0
  let empty = []
  let length x = List.length x

  let push el t = el :: t

  let peek t =
    match t with
    | []     -> None
    | x :: _ -> Some(x)

  let iter t ~f = List.iter t f
end

module VariantStack: MyStack = struct
  type 'a t = Empty | Val of 'a * 'a t

  let is_empty t = 
    match t with
    | Empty -> true
    | _ -> false

  let empty = Empty

  let length t =
    let rec len t acc =
      match t with
      | Empty -> acc
      | Val(v, rest) -> len rest acc + 1
    in len t 0

  let push el t = Val(el, t)
  
  let peek t = 
    match t with
    | Empty     -> None
    | Val(v, _) -> Some(v)
  
  let rec iter t ~f = 
    match t with
    | Empty -> ()
    | Val(v, rest) -> f v; iter rest f
end

let () = 
  VariantStack.empty
  |> VariantStack.push 1
  |> VariantStack.push 2
  |> VariantStack.iter ~f:(fun x -> Stdio.printf "%d \n" x)