//Interfaces and different implementatiuons of polymorphal list
//            by Alexander Chebykin

//Task 27
type IMyList<'A when 'A: equality> = 
  interface
    abstract AddFront : 'A -> IMyList<'A>
    abstract AddBack  : 'A -> IMyList<'A>
    abstract AddAt    : int -> 'A -> IMyList<'A>
    abstract DelFront : IMyList<'A>
    abstract DelBack  : IMyList<'A>
    abstract DelAt    : int -> IMyList<'A>
    abstract Find     : 'A -> bool
    abstract Concat   : IMyList<'A> -> IMyList<'A>
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
        ATDList<'A> (Cons (value, list)) :> IMyList<'A>

      override self.AddBack value =
        let rec addBack x list =
          match list with
          | Nil -> Cons (x, Nil)
          | Cons (a, t) -> Cons (a, addBack x t)

        ATDList<'A> (addBack value list) :> IMyList<'A>

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

        ATDList<'A> (addAt (index - 1) value list) :> IMyList<'A>

      override self.DelFront =
        let a = match list with
                | Nil -> Nil
                | Cons (x, l) -> l

        ATDList<'A> (a) :> IMyList<'A>

      override self.DelBack = 
        let rec delBack list =
          match list with
          | Nil -> Nil
          | Cons (x, Nil) -> Nil
          | Cons (x, l) -> Cons (x, delBack l)

        ATDList<'A> (delBack list) :> IMyList<'A>
      
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

        ATDList<'A> (delAt (index - 1) list) :> IMyList<'A>

      override self.Find value =
        let rec find x list =
          match list with
          | Nil -> false
          | Cons(a, t) ->
            if (a = x) then true
                       else find x t
        find value list

      override self.Concat (list2) = 
        let rec concat list = 
          match list with
          | Nil -> (list2 :?> ATDList<'A>).Self
          | Cons (a, l) -> Cons (a, concat l)
        ATDList<'A> (concat list)  :> IMyList<'A>
    end
  end

type ArrayList<'A when 'A : equality> (ar : 'A[], n) = 
  class
    let mutable array = ar
    let last = n
    let IncreaseNum = 9 // Number of elemnts by which we shall expand array at one time
    member self.Print =
      printfn "%A" array
    member self.Self = array
    member self.Last = last
    interface IMyList<'A> with
      override self.AddFront value =
        ArrayList<'A> (Array.append [|value|] array, last + 1) :> IMyList<'A>
      override self.AddBack value = 
        ArrayList<'A> (Array.append array [|value|], last + 1) :> IMyList<'A>
      override self.AddAt index value =
        ArrayList<'A> (Array.append (Array.append array.[0..index-1] [|value|]) array.[index..array.Length-1], last + 1) :> IMyList<'A>
      override self.DelFront =
        ArrayList<'A> (array.[1..array.Length - 1], last - 1) :> IMyList<'A>
      override self.DelBack =
        ArrayList<'A> (array.[0..array.Length - 2], last - 1) :> IMyList<'A>
      override self.DelAt index =
        if (index < 0 || index > last) 
          then failwith "There's no such element"
          else
            ArrayList<'A> (Array.append array.[0..index - 1] array.[index + 1..array.Length - 1], last - 1) :> IMyList<'A>
      override self.Find value =
        Array.exists (fun elem -> elem = value) array
      override self.Concat list2 =
        ArrayList<'A> (Array.append array ((list2 :?> ArrayList<'A>).Self), last + (list2 :?> ArrayList<'A>).Last + 1) :> IMyList<'A>
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
  let list = ((((list :> IMyList<int>).AddFront 17)).AddBack 90).AddAt 3 12 :?> ATDList<int>
  list.Print
  printfn "Concatenating with list2 = [4; 5; 6]"
  let list = ((list :> IMyList<int>).Concat list2) :?> ATDList<int>
  list.Print
  printfn "Deleting at the front and at the back"
  let list = ((list :> IMyList<int>).DelFront).DelBack :?> ATDList<int>
  list.Print
  printfn "Deleting at 4th position (counting from 1)"
  let list = (list :> IMyList<int>).DelAt 4 :?> ATDList<int>
  list.Print
  printfn "it is %A that 12 is in the list, and it is %A that 17 is in the list" ((list :> IMyList<int>).Find 12) ((list :> IMyList<int>).Find 17)
  //task 29
  printfn "Array implementation"
  let list = new ArrayList<int> ([|1; 2; 3;|], 2)
  let list2 = new ArrayList<int> ([|4; 5; 6;|], 2)
  printfn "Initial list: "
  list.Print
  printfn "Adding 17 at the front, 12 to the 3rd(counting from 1) position, and 90 at the back: "
  let list = ((((list :> IMyList<int>).AddFront 17)).AddBack 90).AddAt (3 - 1) 12 :?> ArrayList<int>
  list.Print
  printfn "Concatenating with list2 = [4; 5; 6]"
  let list = ((list :> IMyList<int>).Concat list2) :?> ArrayList<int>
  list.Print
  printfn "Deleting at the front and at the back"
  let list = ((list :> IMyList<int>).DelFront).DelBack :?> ArrayList<int>
  list.Print
  printfn "Deleting at 4th position (counting from 1)"
  let list = (list :> IMyList<int>).DelAt (4 - 1)  :?> ArrayList<int>
  list.Print
  printfn "it is %A that 17 is in the list, and it is %A that 12 is in the list" ((list :> IMyList<int>).Find 17) ((list :> IMyList<int>).Find 12)
  0