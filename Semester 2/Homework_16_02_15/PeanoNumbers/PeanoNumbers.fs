// Functions for working with Peano numbers.
//      by Alexander Chebykin
type peano = Zero | S of peano
let suc (p : peano) = S p;
let minus1 (p : peano) = 
    match p with
    | Zero -> Zero
    | S p -> p
let rec plus a b = 
    match a with
    | Zero -> b
    | S a -> S (plus a b)
let rec minus a b = 
    match a, b with
    | Zero, _ -> Zero
    | S a, Zero -> S a
    | S a, S b -> minus a b

let rec peanoToInt a =
    match a with
    | Zero -> 0
    | S a -> 1 + peanoToInt(a)
let rec mul a b = 
    match a,b with
    | Zero, _ -> Zero
    | _, Zero -> Zero
    | _, S b -> plus a (mul a b)

let rec pow a b = 
    match a,b with
    | Zero, _ -> Zero
    | _, Zero -> S (Zero)
    | _, S b -> mul a (pow a b)
[<EntryPoint>]
let main argv = 
    printfn "%A" (peanoToInt (pow (S(S(S (S (Zero))))) (S(S(S (S (Zero)))))))
    0