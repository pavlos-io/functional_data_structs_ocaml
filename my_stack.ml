module type MyStack = sig
  type 'a t
  
  val empty: 'a t
  val length: 'a t -> int
  val push: 'a -> 'a t -> 'a t
  val peek: 'a t -> 'a option
  val iter: 'a t -> f:('a -> unit) -> unit
end

module ListStack: MyStack = struct 
  type 'a t = 'a list

  let empty = []
  let length x = List.length x

  let push el t = el :: t

  let peek t =
    match t with
    | [] -> None
    | x :: _ -> Some(x)

  let iter t ~f = List.iter t f
end

let lst = ListStack.empty
let one = ListStack.push 1 ListStack.empty 

let () = 
  lst
  |> ListStack.push 1
  |> ListStack.push 2
  |> ListStack.iter ~f:(fun x -> Stdio.printf "%d \n" x)