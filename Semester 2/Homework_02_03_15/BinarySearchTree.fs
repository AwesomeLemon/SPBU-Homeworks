//Polymorphal binary search tree and functions to work with it
//      by Alexander Chebykin
// Estimated time: 2hr. Actual time: 9hr.
module tree
type Tree<'A> = Nil | Cons of 'A * Tree<'A> * Tree<'A>
let rec add x t =
  match t with
  | Nil -> Cons (x, Nil, Nil)
  | Cons (c, l, r) ->
      if x < c then Cons (c, add x l, r) else Cons (c, l, add x r)

let combineIntoTree c l r =
   Cons (c, l, r)

let rec map f t = 
  match t with
  | Nil -> Nil
  | Cons(c, l, r) -> Cons(f c, map f l, map f r)

let rec fold f a t = 
  match t with
  | Nil -> a
  | Cons(c, l, r) -> f c (fold f a l)  (fold f a r)

let sum t = 
  match t with
  | Nil -> None
  | t -> 
    let zero = 0.0
    let res = fold (fun a la ra -> a + la + ra ) zero t
    Some res

let minOfTree t =
  match t with
  | Nil -> None
  | Cons(c, l, r) -> 
    let res = fold (fun a la ra -> min (min a la) (min a ra)) c t
    Some res

let clone t =
  match t with
  | Nil -> None
  | t -> 
    let res = fold combineIntoTree Nil t
    Some res

let printTree t =
  let rec printT t = 
      match t with
      | Nil -> printf ""
      | Cons(c, l, r) -> 
          printf "Node %A {" c
          match l with
          | Nil -> printf "Leaf"
          | l -> printT l
          printf "; "
          match r with
          | Nil -> printf "Leaf"
          | l -> printT r
          printf "}"
  printT t
  printf "\n"

[<EntryPoint>]
let main argv = 
    //Task 14: polymorphal tree
    printfn "Adding 17.7 12.0 1000.0 300.0 14.0 10000.0"
    let tree = (add 10000.0 (add 14.0 (add 300.0 (add 1000.0(add 12.0 (add 17.7 Nil))))))
    printTree tree
    //Task 15: map for polymorphal tree
    let mul = 2.0
    printfn "Map: multiply by %A" mul
    let tree = map (fun x -> x * mul) tree
    printTree tree
    //Task 16 is integrated into tasks 17 - 19
    //Task 17: sum of all values in tree via fold
    match sum tree with 
    | None -> printfn "The tree is empty"
    | Some x -> printfn "Sum of elements equals %A" x
    //Task 18: minimal element in tree via fold
    match minOfTree tree with
    | None -> printfn "The tree is empty"
    | Some x -> printfn "Minimal element equals %A" x
    //Task 19: clone tree via fold
    match clone tree with 
    | None -> printfn "The tree is empty"
    | Some x -> 
        printfn "The cloned tree (obviously) is"
        printTree x
    0