//Polymorphal binary search tree and functions to work with it
//      by Alexander Chebykin
// Estimated time: 2hr. Actual time: 9.5hr.
module tree
type Tree<'A> = Nil | Cons of 'A * Tree<'A> * Tree<'A>
let rec add x t =
  match t with
  | Nil -> Cons (x, Nil, Nil)
  | Cons (c, l, r) ->
      if x < c then Cons (c, add x l, r) else Cons (c, l, add x r)

let rec findSmallest t = 
  match t with
  | Cons (c, Nil, _) -> Some c
  | Cons (c, l, _) -> findSmallest l
  | Nil -> None 

let rec del x t = 
  match t with
  | Cons (c, Nil, Nil) -> Nil
  | Cons (c, l, r) ->
    if x < c then Cons (c, del x l, r)
    else if x > c then  Cons (c, l, del x r) 
    else 
      match l,r with
      | l, Nil -> l // if both l and r are "Nil" this option will still give the right answer "Nil"
      | Nil, r -> r
      | l, r -> 
          let temp = findSmallest r
          match temp with
          | Some x -> Cons (x, l, del x r)
          | None -> t //This line will never be executed ('cause it can be only if r = Nil, and we've already seen to that case), but if it will, it'd mean that deletion has gone wrong and it's only logical to return initial tree
  | Nil -> Nil

let treeWalk mode t= 
    let rec lcr t = 
        match t with
        | Nil -> printf ""
        | Cons (c, t1, t2) ->                                    
            lcr t1
            printf "%i " c
            lcr t2

    let rec lrc t = 
        match t with
        | Nil -> printf ""
        | Cons (c, t1, t2) ->                                  
            lrc t1
            lrc t2
            printf "%i " c  

    let rec clr t = 
        match t with
        | Nil -> printf ""
        | Cons (c, t1, t2) ->         
            printf "%i " c                           
            clr t1
            clr t2
    printf "%s: " mode
    match mode with
    | "lrc" -> lrc t
    | "lcr" -> lcr t
    | "clr" -> clr t
    | _ -> printf "Error: there's no such tree-walk as %s" mode
    printf "\n"



let rec map f t = 
  match t with
  | Nil -> Nil
  | Cons(c, l, r) -> Cons(f c, map f l, map f r)

let rec fold f a t = 
  match t with
  | Nil -> a
  | Cons(c, l, r) -> f c (fold f a l)  (fold f a r)

let sum t = 
   fold (fun a la ra -> a + la + ra ) 0.0 t

let minOfTree t =
    fold (fun a la ra -> 
            match la, ra with 
            | None, None -> Some a
            | None, Some x -> Some (min x a)
            | Some x, None -> Some (min x a)
            | Some x, Some y -> Some (min (min x a) y)) None t

let combineIntoTree c l r =
   Cons (c, l, r)

let clone t =
  fold combineIntoTree Nil t

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
    printfn "Sum of elements equals %A" (sum tree)
    //Task 18: minimal element in tree via fold
    match minOfTree tree with
    | None -> printfn "The tree is empty"
    | Some x -> printfn "Minimal element equals %A" x
    //Task 19: clone tree via fold
    match tree with 
    | Nil -> printfn "The tree is empty"
    | tree -> 
        printfn "The cloned tree (obviously) is"
        printTree (clone tree)
    0