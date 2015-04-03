//Interfaces and different implementatiuons of polymorphal list
//            by Alexander Chebykin

//Task 27
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

        list <- addAt (index - 1) value list

      override self.DelFront =
        let a = match list with
                | Nil -> Nil
                | Cons (x, l) -> l

        list <- a

      override self.Pop =
        let (a, b) = match list with
                | Nil -> failwith "There's no frist value"
                | Cons (x, l) -> x, l
        list <- b
        a

      override self.DelBack = 
        let rec delBack list =
          match list with
          | Nil -> Nil
          | Cons (x, Nil) -> Nil
          | Cons (x, l) -> Cons (x, delBack l)

        list <- delBack list
      
      override self.DelAt index =
        let rec delAt count list = 
          if count = 0 
            then
              match list with
              | Cons (a, Cons (b, l)) -> Cons (b,l)
              | Cons (a, Nil) -> failwith "Nothing to delete"
              | Nil -> Nil
            else
              match list with
              | Nil -> Nil
              | Cons (a, t) -> Cons (a, delAt (count - 1) t)

        list <- delAt (index - 1) list

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
    let IncreaseNum = 9 // Number of elemnts by which we shall expand array at one time
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
        array <- Array.append (Array.append array.[0..index-1] [|value|]) array.[index..array.Length-1]
        last <- last + 1
      override self.DelFront =
        array <- array.[1..array.Length - 1]
        last <- last - 1
      override self.Pop =
        let res = array.[0]
        array <- array.[1..array.Length - 1]
        last <- last - 1
        res
      override self.DelBack =
        array <- array.[0..array.Length - 2]
        last <- last - 1
      override self.DelAt index =
        if (index < 0 || index > last) 
          then failwith "There's no such element"
          else
            array <- Array.append array.[0..index - 1] array.[index + 1..array.Length - 1]
            last <- last - 1
      override self.Find value =
        Array.exists (fun elem -> elem = value) array
      override self.Concat list2 =
        while not(list2.IsEmpty) do
          (self :> IMyList<'A>).AddBack list2.Pop
     //   ArrayList<'A> (Array.append array ((list2 :?> ArrayList<'A>).Self), last + (list2 :?> ArrayList<'A>).Last + 1) :> IMyList<'A>
      override self.IsEmpty =
        match last with
        | -1 -> true
        | _ -> false
  end
[<EntryPoint>]
let main argv =
  //task 28
  printfn "ATD implementation"
  let list = new ATDList<int> (Cons (1, Cons (2, Cons (3, Nil))))
  let list2 = new ATDList<int> (Cons (4, Cons (5, Cons (6, Nil))))
  printfn "Initial list: "
  list.Print
  printfn "Adding 17 at the front, 12 to the 3rd(counting from 1) position, and 90 at the back: "
  (list :> IMyList<int>).AddFront 17
  (list :> IMyList<int>).AddBack 90
  (list :> IMyList<int>).AddAt 3 12
  list.Print
  printfn "Concatenating with list2 = [4; 5; 6]"
  (list :> IMyList<int>).Concat list2
  list.Print
  printfn "Deleting at the front and at the back"
  (list :> IMyList<int>).DelFront
  (list :> IMyList<int>).DelBack
  list.Print
  printfn "Deleting at 4th position (counting from 1)"
  (list :> IMyList<int>).DelAt 4
  list.Print
  printfn "it is %A that 12 is in the list, and it is %A that 17 is in the list" 
    ((list :> IMyList<int>).Find 12) ((list :> IMyList<int>).Find 17)
  //task 29
  printfn "Array implementation"
  let list = new ArrayList<int> ([|1; 2; 3;|], 2)
  let list2 = new ArrayList<int> ([|4; 5; 6;|], 2)
  printfn "Initial list: "
  list.Print
  printfn "Adding 17 at the front, 12 to the 3rd(counting from 1) position, and 90 at the back: "
  (list :> IMyList<int>).AddFront 17
  (list :> IMyList<int>).AddBack 90
  (list :> IMyList<int>).AddAt (3 - 1) 12
  list.Print
  printfn "Concatenating with list2 = [4; 5; 6]"
  (list :> IMyList<int>).Concat list2
  list.Print
  printfn "Deleting at the front and at the back"
  (list :> IMyList<int>).DelFront
  (list :> IMyList<int>).DelBack
  list.Print
  printfn "Deleting at 4th position (counting from 1)"
  (list :> IMyList<int>).DelAt (4 - 1)
  list.Print
  printfn "it is %A that 17 is in the list, and it is %A that 12 is in the list" 
    ((list :> IMyList<int>).Find 17) ((list :> IMyList<int>).Find 12)
  0