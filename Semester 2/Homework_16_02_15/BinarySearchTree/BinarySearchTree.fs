//Binary search tree and functions to work with it
//      by Alexander Chebykin
// Estimated time: 1hr. Actual time: 2.5 hr. (Numbers are approximate)
type tree = Nil | Cons of int * tree * tree
let rec add x t =
    match t with
    | Nil -> Cons (x, Nil, Nil)
    | Cons (c, l, r) ->
        if x < c then Cons (c, add x l, r) else Cons (c, l, add x r)

let rec findSmallest t = 
    match t with
    | Cons (c, Nil, _) -> c
    | Cons (c, l, _) -> findSmallest l
    | Nil -> exit(1) //There is nor smallest, nor biggest value in an empty tree, so function cannot be calculated, and that's an error

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
            | l, r -> Cons (findSmallest r, l, del (findSmallest r) r)
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

let printTree t =
    let rec printT t = 
        match t with
        | Nil -> printf ""
        | Cons(c, l, r) -> 
            printf "Node %i {" c
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
    printf "Adding 17, 12, 14\n"
    let t = (add 14 (add 12 (add 17 Nil)))
    printTree t
    printf "Adding  13, 39, 30, 35, 6\n"
    let t = (add 6(add 35(add 30(add 39(add 13 t)))))
    printTree t
    printf "Deleting 12\n"
    let t = del 12 t
    printTree t
    printf "Deleting 17\n"
    let t = del 17 t
    printTree t
    treeWalk "lrc" t
    treeWalk "lcr" t
    treeWalk "clr" t
    0 
