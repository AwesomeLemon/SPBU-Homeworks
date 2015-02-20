//Binary search tree and functions to work with it
//      by Alexander Chebykin
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

let rec delHelp x t = 
    match t with
    |Cons (c, Cons(c1, l1, r1), r) ->
        if c1 = x then Cons(c, r1, r) else
            Cons (c, delHelp x (Cons(c1, l1, r1)), r)
    | Cons(c, Nil, r) -> Cons(c, Nil, r)
    | Nil -> Nil

let rec del x t = 
    match t with
    | Cons (c, Nil, Nil) -> Nil
    | Cons (c, Cons(c1, l1, r1), Nil) -> 
        if x = c then Cons(c1, l1, r1) else 
            Cons (c, del x (Cons(c1, l1, r1)), Nil)
    | Cons (c, Nil, Cons(c1, l1, r1)) -> 
        if x = c then Cons(c1, l1, r1) else 
            Cons (c, Nil, del x (Cons(c1, l1, r1)))
    | Cons (c, l, r) ->
        if x = c then Cons (findSmallest r, l, delHelp (findSmallest r) r) else 
            if x < c then Cons (c, del x l, r) else Cons (c, l, del x r)
    | Nil -> Nil

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

[<EntryPoint>]
let main argv = 
    lrc (add 6(add 35(add 30(add 39(add 13(add 14 (add 12 (add 17 Nil))))))))
    printf "\n"
    lrc (del 39 (add 6(add 35(add 30(add 39(add 13(add 14 (add 12 (add 17 Nil)))))))))
    0 
