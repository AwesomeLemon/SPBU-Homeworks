module TreeWorkflow

type tree<'A> = Nil | Node of 'A * tree<'A> * tree<'A>

let (/) x y =
   match y with
   | 0 -> Nil
   | _ -> Node (x / y, Nil, Nil)

let (%) x y =
   match y with
   | 0 -> Nil
   | _ -> Node (x % y, Nil, Nil)
//In order to work with all arithmetic operations in the same way 
//    we have to make them all (int -> int -> tree<int>)
let (+) x y = Node (x + y, Nil, Nil)
let (-) x y = Node (x - y, Nil, Nil)
let (*) x y = Node (x * y, Nil, Nil)

let addTree s t = //adds BST 's' to BST 't'

  let rec addNode x t =
    match t with
    | Nil -> Node (x, Nil, Nil)
    | Node (c, l, r) ->
      if x < c then Node (c, addNode x l, r) else Node (c, l, addNode x r)

  let rec oneStep s t = // one step of addition
    match s with
    | Node (x, l, r) -> 
      let mutable t = addNode x t
      t <- oneStep l t
      t <- oneStep r t
      t
    | Nil -> t

  let mutable res = Nil
  match s, t with
  | Node (x, l, r), Node (y, w, e) ->
    if (x < y) then res <- (oneStep t s)
    else res <- (oneStep s t)
  | Node (x, l, r), Nil -> res <- s
  | Nil, Node (x, l, r) -> res <- t
  | Nil, Nil -> ()
  
  res

let rec map (f: int -> tree<int>) (t : tree<int>) = 

  let combineIntoTree c l r = 
        //After applying f to tree, there's a possibility that it won't be BST no more. 
       //This function remakes it into the BST (not balanced one, in most cases)
    let mutable temp = addTree r l
    temp <- addTree (Node(c, Nil, Nil)) temp
    temp

  match t with
  | Nil -> Nil
  | Node(c, l, r) -> 
    combineIntoTree (f c) (map f l) (map f r)

type TreeBuilder() = 
  member this.Yield (x : 'A) =
    Node (x, Nil, Nil)
  member this.Yield (x : tree<'A>) =
    x
  member this.Zero () =
    Nil
  member this.YieldFrom (x) = x
  member this.For(m,f) =
    this.Bind(m,f)
  member this.Combine (a, b) =
   addTree a (b())
  member this.Delay (f) =
    f
  member this.Bind (m, f) =
    let rec unwrap t =
      match t with
      | Node (Node (x, intL, intR), l, r) -> //'intL' stands for 'left branch in the tree of int'
        let unwrL = unwrap l //'unwrL' stands for 'unwrapped left branch from the tree of trees'
        let unwrR = unwrap r
        let mutable res1 = intL
        let mutable res2 = intR

        match unwrL with
        | Node (c, y, z) ->
          if (c < x) then
            res1 <- addTree unwrL intL
          else
            res1 <- addTree unwrL intR
        | Nil -> ()

        match unwrR with
        | Node (c, y, z) ->
          if (c < x) then
            res2 <- addTree unwrR intL
          else
            res2 <- addTree unwrR intR
        | Nil -> ()

        Node (x, res1, res2)
      | Node (Nil, l, r) ->
        let unwrL = unwrap l
        let unwrR = unwrap r
        addTree unwrL unwrR
      | _ -> Nil

    unwrap (map f m)

let calcTree t1 t2 f = // 'f' is some arithmetical binary operation on ints or option ints
  let res = TreeBuilder () {
    for i in t1 do
      for j in t2 do
       yield! (f i j)
  }
  res()

let mapTree t f =
  let res = TreeBuilder () {
    for i in t do
      yield! (f i)
  }
  res ()
let filterTree t f =
 let res = TreeBuilder () {
    for i in t do
     if (f i) then
        yield i
      else 
        yield! Nil
 }
 res ()

[<EntryPoint>]
let main argv = 
  0 