//Interfaces and different implementatiuons of polymorphal list and lots of tests for them
//            by Alexander Chebykin

//Task 27
module from27to29
open NUnit.Framework
exception Error of string
exception ErrorWithParam of string * string
type IMyList<'A when 'A: equality> = 
  interface
    abstract AddFront : 'A -> unit
    abstract AddBack  : 'A -> unit
    abstract AddAt    : int -> 'A -> unit
    abstract DelFront : unit
    abstract DelBack  : unit
    abstract DelAt    : int -> unit
    abstract Find     : 'A -> bool
    abstract Concat   : IMyList<'A> -> unit
    abstract IsEmpty  : bool
    abstract Pop      : 'A
  end

type list<'A> = Nil | Cons of 'A * list<'A>

type ATDList<'A when 'A : equality> (l) = 
  class
    let mutable list = l
    let rec listPrint l = 
      match l with
      | Nil -> printf "\n"
      | Cons (x, l) -> 
      printf "%A " x
      listPrint l
    member self.Print = 
      listPrint list
    member self.Self = list
    interface IMyList<'A> with
      override self.AddFront value = 
        list <- Cons (value, list)

      override self.AddBack value =
        let rec addBack x list =
          match list with
          | Nil -> Cons (x, Nil)
          | Cons (a, t) -> Cons (a, addBack x t)

        list <- addBack value list

      override self.AddAt index value=
        let rec addAt count x list = 
          if count = 0 
            then
              match list with
              | Cons (a, l) -> Cons (x, Cons (a, l))
              | Nil -> Cons (x, Nil)
            else
              match list with
              | Nil -> Cons (x, Nil)
              | Cons (a, t) -> Cons (a, addAt (count - 1) x t)
        if (index >= 0) then 
            list <- addAt (index) value list

      override self.DelFront =
        let a = match list with
                | Nil -> raise (Error ("There's no first value"))
                | Cons (x, l) -> l

        list <- a

      override self.Pop =
        let (a, b) = match list with
                     | Nil -> raise (Error ("There's no first value"))
                     | Cons (x, l) -> x, l
        list <- b
        a

      override self.DelBack = 
        let rec delBack list =
          match list with
          | Nil -> raise (Error ("There's no last value (the list is empty"))
          | Cons (x, Nil) -> Nil
          | Cons (x, l) -> Cons (x, delBack l)

        list <- delBack list
      
      override self.DelAt index =
        let rec delAt count list = 
          if count = 0 
            then
              match list with
              | Cons (a, Cons (b, l)) -> Cons (b,l)
              | Cons (a, Nil) -> Nil
              | Nil -> raise (ErrorWithParam ("DelAt: There's no such index in the list: ", index.ToString()))
            else
              match list with
              | Nil -> raise (ErrorWithParam ("DelAt: There's no such index in the list: ", index.ToString()))
              | Cons (a, t) -> Cons (a, delAt (count - 1) t)

        list <- delAt index list

      override self.Find value =
        let rec find x list =
          match list with
          | Nil -> false
          | Cons(a, t) ->
            if (a = x) then true
                       else find x t
        find value list

      override self.Concat list2 =
        while not(list2.IsEmpty) do
          (self :> IMyList<'A>).AddBack list2.Pop
      override self.IsEmpty =
        match list with 
        | Nil -> true
        | _ -> false
    end
  end

type ArrayList<'A when 'A : equality> (ar : 'A[], n) = 
  class
    let mutable array = ar
    let mutable last = n
    let IncreaseNum = 9 // Number of elements by which we shall expand array at one time
    member self.Print =
      printfn "%A" array
    member self.Self = array
    member self.Last = last
    interface IMyList<'A> with
      override self.AddFront value =
        array <- Array.append [|value|] array
        last <- last + 1
      override self.AddBack value = 
        array <- Array.append array [|value|]
        last <- last + 1
      override self.AddAt index value =
        if (index <= last + 1) && (index >= 0) then
            array <- Array.append (Array.append array.[0..index-1] [|value|]) array.[index..array.Length-1]
            last <- last + 1
      override self.DelFront =
        if not((self :> IMyList<'A>).IsEmpty) then
            array <- array.[1..array.Length - 1]
            last <- last - 1
        else raise (Error ("There's no first value"))
      override self.Pop =
        if (self :> IMyList<'A>).IsEmpty then raise (Error ("There's no first value"))
        let res = array.[0]
        array <- array.[1..array.Length - 1]
        last <- last - 1
        res
      override self.DelBack =
        if not((self :> IMyList<'A>).IsEmpty) then
            array <- array.[0..array.Length - 2]
            last <- last - 1
        else raise (Error ("There's no last value (the list is empty"))
      override self.DelAt index =
        if (index = 0 && index <= last) then 
            (self :> IMyList<'A>).DelFront
        else
            if (index > 0 && index <= last) then 
                array <- Array.append array.[0..index - 1] array.[index + 1..array.Length - 1]
                last <- last - 1
            else raise (ErrorWithParam ("DelAt: There's no such index in the list: ", index.ToString()))
      override self.Find value =
        Array.exists (fun elem -> elem = value) array
      override self.Concat list2 =
        while not(list2.IsEmpty) do
          (self :> IMyList<'A>).AddBack list2.Pop
      override self.IsEmpty =
        match last with
        | -1 -> true
        | _ -> false
  end

//Array implementation:
//AddFront
[<Test>]
let ``Adding at front of zero list`` () = 
  let list = new ArrayList<int> ([||], -1)
  (list :> IMyList<int>).AddFront 17
  Assert.AreEqual(list.Self, [|17|])

[<Test>]
let ``Adding at front of non-zero list`` () = 
  let list = new ArrayList<int> ([|1; 2; 3;|], 2)
  (list :> IMyList<int>).AddFront 17
  Assert.AreEqual(list.Self, [|17; 1; 2; 3|])

[<Test>]
let ``Adding at front of non-zero list twice`` () = 
  let list = new ArrayList<int> ([|1; 2; 3;|], 2)
  (list :> IMyList<int>).AddFront 17
  (list :> IMyList<int>).AddFront 49
  Assert.AreEqual(list.Self, [|49; 17; 1; 2; 3|])
//AddBack
[<Test>]
let ``Adding at back of zero list`` () = 
  let list = new ArrayList<int> ([||], -1)
  (list :> IMyList<int>).AddBack 17
  Assert.AreEqual(list.Self, [|17|])
[<Test>]
let ``Adding at back of non-zero list`` () = 
  let list = new ArrayList<int> ([|1; 2; 3;|], 2)
  (list :> IMyList<int>).AddBack 17
  Assert.AreEqual(list.Self, [|1; 2; 3; 17|])

[<Test>]
let ``Adding at back of non-zero list twice`` () = 
  let list = new ArrayList<int> ([|1; 2; 3;|], 2)
  (list :> IMyList<int>).AddBack 17
  (list :> IMyList<int>).AddBack 49
  Assert.AreEqual(list.Self, [|1; 2; 3; 17; 49|])
//AddAt
[<Test>]
let ``Adding at 0 position of zero list`` () = 
  let list = new ArrayList<int> ([||], -1)
  (list :> IMyList<int>).AddAt 0 17
  Assert.AreEqual(list.Self, [|17|])

[<Test>]
let ``Adding at 5 position of zero list`` () = 
  let list = new ArrayList<int> ([||], -1)
  (list :> IMyList<int>).AddAt 5 17
  Assert.AreEqual(list.Self, [||])

[<Test>]
let ``Adding at 1 existing and 1 non-existing position of non-zero list`` () = 
  let list = new ArrayList<int> ([|1; 2; 3;|], 2)
  (list :> IMyList<int>).AddAt 1 17
  (list :> IMyList<int>).AddAt -3 78
  Assert.AreEqual(list.Self, [|1; 17; 2; 3|])
[<Test>]
let ``Adding at 2 existing positions of non-zero list`` () = 
  let list = new ArrayList<int> ([|1; 2; 3; 4; 5; 6; 189; 1|], 7)
  (list :> IMyList<int>).AddAt 1 17
  (list :> IMyList<int>).AddAt 3 78
  Assert.AreEqual(list.Self, [|1; 17; 2; 78; 3; 4; 5; 6; 189; 1|])
//DelFront
[<Test>]
let ``Deleting at front of zero list`` () = 
  let list = new ArrayList<int> ([||], -1)
  let mutable res = ""
  try
     (list :> IMyList<int>).DelFront
     res <- list.Self.ToString ()
  with
  | Error (msg) -> 
        printf "%A" (msg)
        res <- msg
  Assert.AreEqual(res, "There's no first value")

[<Test>]
let ``Deleting at front of non-zero list`` () = 
  let list = new ArrayList<int> ([|1; 2; 3;|], 2)
  (list :> IMyList<int>).DelFront
  Assert.AreEqual(list.Self, [|2; 3|])

[<Test>]
let ``Deleting at front of list of 1 element twice`` () = 
  let list = new ArrayList<int> ([|1|], 0)
  let mutable res = ""
  try
     (list :> IMyList<int>).DelFront
     (list :> IMyList<int>).DelFront
     res <- list.Self.ToString ()
  with
  | Error (msg) -> 
        printf "%A" (msg)
        res <- msg
  Assert.AreEqual(res, "There's no first value")

//DelBack
[<Test>]
let ``Deleting at back of zero list`` () = 
  let list = new ArrayList<int> ([||], -1)
  let mutable res = ""
  try
     (list :> IMyList<int>).DelBack
     res <- list.Self.ToString ()
  with
  | Error (msg) -> 
        printf "%A" (msg)
        res <- msg
  Assert.AreEqual(res, "There's no last value (the list is empty")

[<Test>]
let ``Deleting at back of non-zero list`` () = 
  let list = new ArrayList<int> ([|1; 2; 3;|], 2)
  (list :> IMyList<int>).DelBack
  Assert.AreEqual(list.Self, [|1; 2|])

[<Test>]
let ``Deleting at back of list of 1 element twice`` () = 
  let list = new ArrayList<int> ([|1|], 0)
  let mutable res = ""
  try
     (list :> IMyList<int>).DelBack
     (list :> IMyList<int>).DelBack
     res <- list.Self.ToString ()
  with
  | Error (msg) -> 
        printf "%A" (msg)
        res <- msg
  Assert.AreEqual(res, "There's no last value (the list is empty")

//DelAt
[<Test>]
let ``Deleting at 0 position of zero list`` () = 
  let list = new ArrayList<int> ([||], -1)
  let mutable res = ""
  try
     (list :> IMyList<int>).DelAt 0 
     res <- list.Self.ToString ()
  with
  | ErrorWithParam (msg, param) -> 
        printf "%A" (msg + param)
        res <- msg + param
  Assert.AreEqual(res, "DelAt: There's no such index in the list: 0")

[<Test>]
let ``Deleting at 0 position of non-zero list`` ()= 
  let list = new ArrayList<int> ([|1; 2; 3;|], 2)
  (list :> IMyList<int>).DelAt 0
  Assert.AreEqual(list.Self, [|2; 3|])

[<Test>]
let ``Deleting at 5 position of zero list`` () = 
  let list = new ArrayList<int> ([||], -1)
  let mutable res = ""
  try
     (list :> IMyList<int>).DelAt 5
     res <- list.Self.ToString ()
  with
  | ErrorWithParam (msg, param) -> 
        printf "%A" (msg + param)
        res <- msg + param
  Assert.AreEqual(res, "DelAt: There's no such index in the list: 5")
[<Test>]
let ``Deleting at 1 existing and 1 non-existing position of non-zero list`` () = 
  let list = new ArrayList<int> ([|1; 2; 3;|], 2)
  let mutable res = ""
  try
     (list :> IMyList<int>).DelAt 1
     (list :> IMyList<int>).DelAt -3
     res <- list.Self.ToString ()
  with
  | ErrorWithParam (msg, param) -> 
        printf "%A" (msg + param)
        res <- msg + param
  Assert.AreEqual(res, "DelAt: There's no such index in the list: -3")

[<Test>]
let ``Deleting at 2 existing positions of non-zero list`` () = 
  let list = new ArrayList<int> ([|1; 2; 3; 4; 5; 6; 189; 1|], 7)
  (list :> IMyList<int>).DelAt 2
  (list :> IMyList<int>).DelAt 0
  Assert.AreEqual(list.Self, [|2; 4; 5; 6; 189; 1|])
//Find
[<Test>]
let ``Find something in zero list`` () = 
  let list = new ArrayList<int> ([||], -1)
  let res = (list :> IMyList<int>).Find 17
  Assert.AreEqual(res, false)
[<Test>]
let ``Find non-existant (in this list) value in non-zero list`` () = 
  let list = new ArrayList<int> ([|1; 2; 4|], 2)
  let res = (list :> IMyList<int>).Find 17
  Assert.AreEqual(res, false)
[<Test>]
let ``Find existant (in this list) value in non-zero list`` () = 
  let list = new ArrayList<int> ([|1; 2; 4; 178423; 43; 2323; 2; 1; 1010|], 8)
  let res = (list :> IMyList<int>).Find 43
  Assert.AreEqual(res, true)
//Concat
[<Test>]
let ``Concating to zero Arraylist zero ATDList`` () = 
  let list = new ArrayList<int> ([||], -1)
  let list2 = new ATDList<int> (Nil)
  (list :> IMyList<int>).Concat list2
  Assert.AreEqual(list.Self, [||])
[<Test>]
let ``Concating to zero Arraylist non-zero ATDList`` () = 
  let list = new ArrayList<int> ([||], -1)
  let list2 = new ATDList<int> (Cons (4, Cons (5, Cons (6, Nil))))
  (list :> IMyList<int>).Concat list2
  Assert.AreEqual(list.Self, [|4; 5; 6|])
[<Test>]
let ``Concating to non-zero Arraylist non-zero ATDList`` () = 
  let list = new ArrayList<int> ([|17; 893; 122; 12312312; 1|], 4)
  let list2 = new ATDList<int> (Cons (4, Cons (5, Cons (6, Nil))))
  (list :> IMyList<int>).Concat list2
  Assert.AreEqual(list.Self, [|17; 893; 122; 12312312; 1; 4; 5; 6|])
[<Test>]
let ``Concating to non-zero Arraylist zero ATDList`` () = 
  let list = new ArrayList<int> ([|17; 893; 122; 12312312; 1|], 4)
  let list2 = new ATDList<int> (Nil)
  (list :> IMyList<int>).Concat list2
  Assert.AreEqual(list.Self, [|17; 893; 122; 12312312; 1|])
[<Test>]
let ``Concating two zero Arraylists`` () = 
  let list = new ArrayList<int> ([||], -1)
  let list2 = new ArrayList<int> ([||], -1)
  (list :> IMyList<int>).Concat list2
  Assert.AreEqual(list.Self, [||])
[<Test>]
let ``Concating two non-zero Arraylists`` () = 
  let list = new ArrayList<int> ([|1; 2|], 1)
  let list2 = new ArrayList<int> ([|17; 85; 48; -635|], 3)
  (list :> IMyList<int>).Concat list2
  Assert.AreEqual(list.Self, [|1; 2; 17; 85; 48; -635|])
[<Test>]
let ``Concating to zero Arraylist non-zero ArrayList`` () = 
  let list = new ArrayList<int> ([||], -1)
  let list2 = new ArrayList<int> ([|17; 85; 48; -635|], 3)
  (list :> IMyList<int>).Concat list2
  Assert.AreEqual(list.Self, [|17; 85; 48; -635|])
[<Test>]
let ``Concating to non-zero Arraylist zero ArrayList`` () = 
  let list = new ArrayList<int> ([||], -1)
  let list2 = new ArrayList<int> ([|17; 85; 48; -635|], 3)
  (list2 :> IMyList<int>).Concat list
  Assert.AreEqual(list2.Self, [|17; 85; 48; -635|])



//ATD implementation

//AddFront
[<Test>]
let ``ATD: Adding at front of zero list`` () = 
  let list = new ATDList<int> (Nil)
  (list :> IMyList<int>).AddFront 17
  Assert.AreEqual(list.Self, Cons(17, Nil))

[<Test>]
let ``ATD: Adding at front of non-zero list`` () = 
  let list = new ATDList<int> (Cons (1, Cons(2, Cons (3, Nil))))
  (list :> IMyList<int>).AddFront 17
  Assert.AreEqual(list.Self, Cons (17, Cons (1, Cons(2, Cons (3, Nil)))))

[<Test>]
let ``ATD: Adding at front of non-zero list twice`` () = 
  let list = new ATDList<int> (Cons (1, Cons(2, Cons (3, Nil))))
  (list :> IMyList<int>).AddFront 17
  (list :> IMyList<int>).AddFront 49
  Assert.AreEqual(list.Self, Cons (49, Cons (17, Cons (1, Cons(2, Cons (3, Nil))))))
//AddBack
[<Test>]
let ``ATD: Adding at back of zero list`` () = 
  let list = new ATDList<int> (Nil)
  (list :> IMyList<int>).AddBack 17
  Assert.AreEqual(list.Self, Cons(17, Nil))
[<Test>]
let ``ATD: Adding at back of non-zero list`` () = 
  let list = new ATDList<int> (Cons (1, Cons(2, Cons (3, Nil))))
  (list :> IMyList<int>).AddBack 17
  Assert.AreEqual(list.Self, Cons (1, Cons(2, Cons (3, Cons (17, Nil)))))

[<Test>]
let ``ATD: Adding at back of non-zero list twice`` () = 
  let list = new ATDList<int> (Cons (1, Cons(2, Cons (3, Nil))))
  (list :> IMyList<int>).AddBack 17
  (list :> IMyList<int>).AddBack 49
  Assert.AreEqual(list.Self, Cons (1, Cons(2, Cons (3, Cons (17, Cons (49, Nil))))))
//AddAt
[<Test>]
let ``ATD: Adding at 0 position of zero list`` () = 
  let list = new ATDList<int> (Nil)
  (list :> IMyList<int>).AddAt 0 17
  Assert.AreEqual(list.Self, Cons(17, Nil))

[<Test>]
let ``ATD: Adding at 5 position of zero list`` () = 
  let list = new ATDList<int> (Nil)
  (list :> IMyList<int>).AddAt 5 17
  Assert.AreEqual(list.Self, Cons (17, Nil))

[<Test>]
let ``ATD: Adding at 1 existing and 1 non-existing position of non-zero list`` () = 
  let list = new ATDList<int> (Cons (1, Cons(2, Cons (3, Nil))))
  (list :> IMyList<int>).AddAt 1 17
  (list :> IMyList<int>).AddAt -3 78
  Assert.AreEqual(list.Self, Cons (1, Cons(17, Cons(2, Cons (3, Nil)))))
[<Test>]
let ``ATD: Adding at 2 existing positions of non-zero list`` () = 
  let list = new ATDList<int> (Cons (1, Cons (2, Cons (3, Cons (4, Cons (5, Cons (6, Cons (189, Cons (1, Nil)))))))))
  (list :> IMyList<int>).AddAt 1 17
  (list :> IMyList<int>).AddAt 3 78
  Assert.AreEqual(list.Self, Cons (1, Cons (17 , Cons (2, Cons (78, Cons (3, Cons (4, Cons (5, Cons (6, Cons (189, Cons (1, Nil)))))))))))
//DelFront
[<Test>]
let ``ATD: Deleting at front of zero list`` () = 
  let list = new ATDList<int> (Nil)
  let mutable res = ""
  try
     (list :> IMyList<int>).DelFront
     res <- list.Self.ToString ()
  with
  | Error (msg) -> 
        printf "%A" (msg)
        res <- msg
  Assert.AreEqual(res, "There's no first value")

[<Test>]
let ``ATD: Deleting at front of non-zero list`` () = 
  let list = new ATDList<int> (Cons (1, Cons(2, Cons (3, Nil))))
  (list :> IMyList<int>).DelFront
  Assert.AreEqual(list.Self, Cons(2, Cons (3, Nil)))

[<Test>]
let ``ATD: Deleting at front of list of 1 element twice`` () = 
  let list = new ATDList<int> (Cons (3, Nil))
  let mutable res = ""
  try
     (list :> IMyList<int>).DelFront
     (list :> IMyList<int>).DelFront
     res <- list.Self.ToString ()
  with
  | Error (msg) -> 
        printf "%A" (msg)
        res <- msg
  Assert.AreEqual(res, "There's no first value")
  
  Assert.AreEqual(list.Self, (Nil : list<int>))
//DelBack
[<Test>]
let ``ATD: Deleting at back of zero list`` () = 
  let list = new ATDList<int> (Nil)
  let mutable res = ""
  try
     (list :> IMyList<int>).DelBack
     res <- list.Self.ToString ()
  with
  | Error (msg) -> 
        printf "%A" (msg)
        res <- msg
  Assert.AreEqual(res, "There's no last value (the list is empty")

[<Test>]
let ``ATD: Deleting at back of non-zero list`` () = 
  let list = new ATDList<int> (Cons (1, Cons(2, Cons (3, Nil))))
  (list :> IMyList<int>).DelBack
  Assert.AreEqual(list.Self, Cons (1, Cons(2, Nil)))

[<Test>]
let ``ATD: Deleting at back of list of 1 element twice`` () = 
  let list = new ATDList<int> (Nil)
  let mutable res = ""
  try
     (list :> IMyList<int>).DelBack
     (list :> IMyList<int>).DelBack
     res <- list.Self.ToString ()
  with
  | Error (msg) -> 
        printf "%A" (msg)
        res <- msg
  Assert.AreEqual(res, "There's no last value (the list is empty")
  
  Assert.AreEqual(list.Self, (Nil : list<int>))
//DelAt
[<Test>]
let ``ATD: Deleting at 0 position of zero list`` () = 
  let list = new ATDList<int> (Nil)
  let mutable res = ""
  try
     (list :> IMyList<int>).DelAt 0
     res <- list.Self.ToString ()
  with
  | ErrorWithParam (msg, param) -> 
        printf "%A" (msg + param)
        res <- msg + param
  Assert.AreEqual(res, "DelAt: There's no such index in the list: 0")

[<Test>]
let ``ATD: Deleting at 0 position of non-zero list`` () = 
  let list = new ATDList<int> (Cons (1, Cons(2, Cons (3, Nil))))
  (list :> IMyList<int>).DelAt 0
  Assert.AreEqual(list.Self, Cons(2, Cons (3, Nil)))

[<Test>]
let ``ATD: Deleting at 5 position of zero list`` () = 
  let list = new ATDList<int> (Nil)
  let mutable res = ""
  try
     (list :> IMyList<int>).DelAt 5
     res <- list.Self.ToString ()
  with
  | ErrorWithParam (msg, param) -> 
        printf "%A" (msg + param)
        res <- msg + param
  Assert.AreEqual(res, "DelAt: There's no such index in the list: 5")
[<Test>]
let ``ATD: Deleting at 1 existing and 1 non-existing position of non-zero list`` () = 
  let list = new ATDList<int> (Cons (1, Cons(2, Cons (3, Nil))))
  let mutable res = ""
  try
     (list :> IMyList<int>).DelAt 1
     (list :> IMyList<int>).DelAt -3
     res <- list.Self.ToString ()
  with
  | ErrorWithParam (msg, param) -> 
        printf "%A" (msg + param)
        res <- msg + param
  Assert.AreEqual(res, "DelAt: There's no such index in the list: -3")
[<Test>]
let ``ATD: Deleting at 2 existing positions of non-zero list`` () = 
  let list = new ATDList<int> (Cons (1, Cons (2, Cons (3, Cons (4, Cons (5, Cons (6, Cons (189, Cons (1, Nil)))))))))
  (list :> IMyList<int>).DelAt 2
  (list :> IMyList<int>).DelAt 0
  Assert.AreEqual(list.Self, (Cons (2, Cons (4, Cons (5, Cons (6, Cons (189, Cons (1, Nil))))))))
//Find
[<Test>]
let ``ATD: Find something in zero list`` () = 
  let list = new ATDList<int> (Nil)
  let res = (list :> IMyList<int>).Find 17
  Assert.AreEqual(res, false)
[<Test>]
let ``ATD: Find non-existant (in this list) value in non-zero list`` () = 
  let list = new ATDList<int> (Cons (1, Cons(2, Cons (3, Nil))))
  let res = (list :> IMyList<int>).Find 17
  Assert.AreEqual(res, false)
[<Test>]
let ``ATD: Find existant (in this list) value in non-zero list`` () = 
  let list = new ATDList<double> (Cons (1.1, Cons (2.2, Cons (3.5, Cons (43.5, Cons (5.77, Cons (6.774, Cons (189.4, Cons (1.4, Nil)))))))))
  let res = (list :> IMyList<double>).Find 43.5
  Assert.AreEqual(res, true) 
//Concat
[<Test>]
let ``ATD: Concating to zero ATDList zero Arraylist`` () = 
  let list = new ArrayList<int> ([||], -1)
  let list2 = new ATDList<int> (Nil)
  (list2 :> IMyList<int>).Concat list
  Assert.AreEqual(list2.Self, (Nil : list<int>))
[<Test>]
let ``ATD: Concating to zero ATDList non-zero Arraylist`` () = 
  let list = new ArrayList<int> ([|4; 5; 6|], 2)
  let list2 = new ATDList<int> (Nil)
  (list2 :> IMyList<int>).Concat list
  Assert.AreEqual(list2.Self, Cons (4, Cons (5, Cons (6, Nil))))
[<Test>]
let ``ATD: Concating to non-zero ATDList non-zero Arraylist`` () = 
  let list = new ArrayList<int> ([|17; 893; 122; 1|], 3)
  let list2 = new ATDList<int> (Cons (4, Cons (5, Cons (6, Nil))))
  (list2 :> IMyList<int>).Concat list
  Assert.AreEqual (list2.Self, Cons (4, Cons (5, Cons (6, Cons (17, Cons (893, Cons (122, Cons (1, Nil))))))))
[<Test>]
let ``ATD: Concating to non-zero ATDlist zero ArrayList`` () = 
  let list = new ArrayList<int> ([||], -1)
  let list2 = new ATDList<int> (Cons (4, Cons (5, Cons (6, Nil))))
  (list2 :> IMyList<int>).Concat list
  Assert.AreEqual(list2.Self, Cons (4, Cons (5, Cons (6, Nil))))
[<Test>]
let ``ATD: Concating two zero ATDlists`` () = 
  let list = new ATDList<int> (Nil)
  let list2 = new ATDList<int> (Nil)
  (list2 :> IMyList<int>).Concat list
  Assert.AreEqual(list2.Self, (Nil : list<int>))
[<Test>]
let ``ATD: Concating two non-zero ATDlists`` () = 
  let list = new ATDList<int> (Cons (1, Cons (2, Nil)))
  let list2 = new ATDList<int> (Cons (17, Nil))
  (list2 :> IMyList<int>).Concat list
  Assert.AreEqual(list2.Self, Cons (17, Cons (1, Cons (2, Nil))))
[<Test>]
let ``ATD: Concating to zero ATDlist non-zero ATDList`` () = 
  let list = new ATDList<int> (Nil)
  let list2 = new ATDList<int> (Cons (4, Cons (5, Cons (6, Nil))))
  (list2 :> IMyList<int>).Concat list
  Assert.AreEqual(list2.Self, Cons (4, Cons (5, Cons (6, Nil))))
[<Test>]
let ``ATD: Concating to non-zero ATDlist zero ATDList`` () = 
  let list = new ATDList<int> (Nil)
  let list2 = new ATDList<int> (Cons (4, Cons (5, Cons (6, Nil))))
  (list2 :> IMyList<int>).Concat list
  Assert.AreEqual(list2.Self, Cons (4, Cons (5, Cons (6, Nil)))) 
let main argv =
  0