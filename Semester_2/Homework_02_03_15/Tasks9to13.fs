﻿//      by Alexander Chebykin
// Estimated time: 1hr. Actual time: 3.5 hr.
module list
type list<'A> = Nil | Cons of 'A * list<'A>

let rec fold f a l = 
  match l with
  | Nil -> a
  | Cons (x, l) -> fold f (f a x) l

let add l x =
  Cons(x, l)

let reverse l =
  fold (add) Nil l

let filterViaFold f l = 
  reverse (fold (fun l x -> if f x then Cons(x, l) else l) Nil l)

let mapViaFold f l =
  reverse (fold (fun t x -> add t (f x)) Nil l)

let rec listPrint l = 
  match l with
  | Nil -> printf "\n"
  | Cons (x, l) -> 
    printf "%A " x
    listPrint l

let gorner co x0= 
  fold (fun a x -> x + a * x0) 0 co

[<EntryPoint>]
let main argv = 
    //Task 9: List.iter type
    //  val it : (('a -> unit) -> 'a list -> unit) = <fun:clo@7>
    //Task 10: list reversion
    let t = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
    printfn "Initial list:"
    listPrint t
    printfn "After reversing:"
    listPrint (reverse t)
    //Task 11: filter via fold
    let t = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
    printfn "Filtering by (x>2):"
    let func x = if x > 2 then true else false
    listPrint (filterViaFold (func) t)
    //Task 12: map via fold
    let t = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
    printfn "Map: multiply by 17:"
    let func x = x * 17
    listPrint (mapViaFold (func) t)
    //Task 13: Gorner's scheme via fold
    let co = Cons(1, Cons(17, Cons(0, Cons(4, Nil))))
    let x0 = 5
    printfn "Gorner's Scheme: Coefficients in decreasing order (e.g. if polynom was 5x^2 + 7 then you'd see 5 0 7) :"
    listPrint co
    printfn "Variable equals %A" x0
    printfn "The result is %A" (gorner co x0)
    0
