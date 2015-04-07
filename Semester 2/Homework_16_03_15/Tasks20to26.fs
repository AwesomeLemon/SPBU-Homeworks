//Different graph interfaces and implementations
//                by Alexander Chebykin
module from20to26
open System
open NUnit.Framework
//Task 20: graph interface
type IGraph  =
  interface
    abstract IsWay : int -> int -> bool
    abstract Size  : int
  end
//Task 25: polymorph marked graph interface
type IGraphMarked<'A> =
  interface
    inherit IGraph
    abstract ValAt : int -> 'A
  end

//Task 21: IGraph implementation with adjacency matrix
type ArrayOrgraph (verges: (int * int) list, numberOfNods) =
  class
    let adjMatrix = Array2D.zeroCreate numberOfNods numberOfNods
    do 
      for (n1, n2) in verges do Array2D.set adjMatrix n1 n2 true
    interface IGraph with
      override s.IsWay a b =
        adjMatrix.[a, b]
      override s.Size = 
        Array2D.length1 adjMatrix
    end
  end
//Task 21: IGraph implementation with adjacency lists
type ListOrgraph (verges: (int * int) list, numberOfNods) = 
  class 
    let nodes = Array.create numberOfNods []
    do 
      for (n1, n2) in verges do nodes.[n1] <- n2 :: nodes.[n1]
    interface IGraph with
      override s.IsWay a b =
        let rec listFind x l = 
          match l with
          | [] -> None
          | a :: l -> 
            if (x = a) then Some x
               else listFind x l
        listFind b (nodes.[a]) = Some b

      override s.Size =
        nodes.GetLength 0
    end
  end
//Task 23
let canGoTo (graph : IGraph) init = 
  let visited = Array.create graph.Size false
  let rec find (visited :bool[]) cur = 
    let mutable res = []
    for i = 0 to (graph.Size - 1) do
      if (graph.IsWay cur i) then 
        if (visited.[i] = false) then 
          visited.[i] <- true
          res <- List.concat [(i :: res); (find visited i)]
    res
  let mutable res = []
  if (init <= (graph.Size - 1)) && init >= 0 then 
        visited.[init] <- true
        res <- find visited init
  List.sort (res)
//Task 24
let canGetFrom (graph : IGraph) init = 

  let rec find (visited :bool[]) cur target = 
    let mutable res = false
    for i = 0 to (graph.Size - 1) do
      if (graph.IsWay cur i) then 
        if (visited.[i] = false) then 
          if (i = target) then res <- true
          else
            visited.[i] <- true
            res <- res || (find visited i target)
    res

  let mutable res = []
  for i = 0 to (graph.Size - 1) do
    if not (i = init) then
       let visited = Array.create graph.Size false
       visited.[i] <- true
       if (find visited i init) then res <- i :: res
  List.sort res

[<Test>]
let ``t23: No verges, Existing node`` ()= 
  let gr = new ArrayOrgraph ([], 8)
  let res = canGoTo gr 3
  Assert.AreEqual (res, [])

[<Test>]
let ``t23:No verges, Non-existing node`` ()= 
  let gr = new ArrayOrgraph([], 8)
  let res = canGoTo gr -5
  Assert.AreEqual (res, [])

[<TestFixture>]
type UsualGraph () = 
  let ar = [(0, 1); (1, 2); (2, 3); (2, 4); (3, 4); (1, 5); (1, 6); (5, 7); (4, 5)]
  let createUsGr = new ArrayOrgraph (ar, 8)
  [<Test>]
    member self.``t23: Usual graph, Non-existing node`` ()= 
      let gr = createUsGr
      let res = canGoTo gr 30
      Assert.AreEqual (res, [])

  [<Test>]
    member self.``t23: Usual graph, node with nowhere to go`` ()= 
      let gr = createUsGr
      let res = canGoTo gr 7
      Assert.AreEqual (res, [])

  [<Test>]
    member self.``t23: Usual graph, node with somewhere to go`` ()= 
      let gr = createUsGr
      let res = canGoTo gr 3
      Assert.AreEqual (res, [4; 5; 7])

  [<Test>]
    member self.``t24: Usual graph, Non-existing node`` ()= 
      let gr = createUsGr
      let res = canGetFrom gr 30
      Assert.AreEqual (res, [])

  [<Test>]
    member self.``t24: Usual graph, node with nowhere to get from`` ()= 
      let gr = createUsGr
      let res = canGetFrom gr 0
      Assert.AreEqual (res, [])

  [<Test>]
    member self.``t24: Usual graph, node with somewhere to get from`` ()= 
      let gr = createUsGr
      let res = canGetFrom gr 3
      Assert.AreEqual (res, [0; 1; 2])

[<Test>]
let ``t24: No verges, Existing node`` ()= 
  let gr = new ListOrgraph([], 8)
  let res = canGetFrom gr 3
  Assert.AreEqual (res, [])

[<Test>]
let ``t24: No verges, Non-existing node`` ()= 
  let gr = new ListOrgraph([], 8)
  let res = canGetFrom gr -5
  Assert.AreEqual (res, [])
// task 26
type Computer(os)=
  class
    let name = os
    let chanse =
     match name with
     | "Windows" -> 0.4
     | "Linux" -> 0.1
     | "OS X" -> 0.2
     | _ -> 0.5
    let mutable infection = 0 
    member self.Print = 
      printf "%A" infection
    member self.InfectionChanse = chanse
    member self.Infection = infection
    member self.ChangeInfection value = //0 - healthy; 1 - has been infected on the current move (will become viral on the next move); 2 - viral
      infection <- value
  end

type ArrayOrgraphMarked<'A> (verges: (int * int) list, numberOfNods, vals: 'A[], m : int[]) =
  class
    let nodeVals = vals
    let adjMatrix = Array2D.zeroCreate numberOfNods numberOfNods
    let marks = m
    do 
      for (n1, n2) in verges do Array2D.set adjMatrix n1 n2 true
    interface IGraphMarked<'A> with
      override s.IsWay a b =
        if (adjMatrix.[a, b] = true) then true else false
      override s.Size = 
        Array2D.length1 adjMatrix
      override s.ValAt a = 
        nodeVals.[m.[a]]
    end
  end
let a = System.Random()
type MyRandom() = 
  class
    member self.giveRandom x = 
      match x with
      | 1 -> 1.0 
      | 0 -> 0.0
      | _ -> a.NextDouble()
  end

type Net(verges: (int * int) list, numberOfNods, OSs :string[]) = 
  class
    inherit ArrayOrgraphMarked<Computer> (verges, numberOfNods, 
        [| for i in 0 .. OSs.Length - 1 -> new Computer(OSs.[i]) |], [| for i in 0 .. OSs.Length - 1 -> i |])

    member self.PrintInfection = 
      for i = 0 to numberOfNods - 1 do
        printf "%i " ((self  :> IGraphMarked<Computer>).ValAt i).Infection
      printfn ""
    member self.emulateNextStep randVal = 
      let random = MyRandom()
      let graph = self :> IGraphMarked<Computer>
      for i = 0 to graph.Size - 1 do
        if (graph.ValAt i).Infection = 2 then 
          for j = 0 to graph.Size - 1 do
            if ((graph.IsWay i j) && 
                ((graph.ValAt j).Infection = 0) &&
                  (random.giveRandom randVal < (graph.ValAt j).InfectionChanse)) 
                    then (graph.ValAt j).ChangeInfection 1
      // And now infected Computers are getting completely viral so that on the next move they'll be able to infect their neighbours:
      for i = 0 to graph.Size - 1 do
       if (graph.ValAt i).Infection = 1 then (graph.ValAt i).ChangeInfection 2
    member self.Print =
      let graph = self :> IGraphMarked<Computer>
      let c = [|for i in 0 .. graph.Size - 1 -> if (graph.ValAt i).Infection = 2 then "Infected" else "Healthy"|]
      printf "
        (0. Windows%s) --> (1. Linux   %s) --> (2. OS X %s) --> (3. Linux    %s)
                                      |                  |                       |
                                      |                  |                       |
                                     \/                 \/                       \/
                         (6. Windows     %s) --> (4. Unknown    %s) <---(5. Windows   %s)
                          |
                          |
                          \/
                         (7. OS X %s)
        \n\n" c.[0] c.[1] c.[2] c.[3] c.[6] c.[4] c.[5] c.[7]
    member self.InfectionToArray = 
      [|for i in 0 .. numberOfNods - 1 ->
        ((self  :> IGraphMarked<Computer>).ValAt i).Infection
      |]
  end

[<TestCase(0, 1, 1, Result = [|0; 2; 2; 0; 0; 0; 2; 0;|], TestName = "t26: Infection probability is 100%, Starting point 1, 1 move")>]
[<TestCase(0, 1, 3, Result = [|0; 2; 2; 2; 2; 2; 2; 2;|], TestName = "t26: Infection probability is 100%, Starting point 1, 3 moves")>]
[<TestCase(0, 4, 100, Result = [|0; 0; 0; 0; 2; 0; 0; 0;|], TestName = "t26: Infection probability is 100%, Starting point 4, 100 moves")>]
[<TestCase(0, 6, 100, Result = [|0; 0; 0; 0; 2; 0; 2; 2;|], TestName = "t26: Infection probability is 100%, Starting point 6, 100 moves")>]
[<TestCase(1, 1, 1, Result = [|0; 2; 0; 0; 0; 0; 0; 0;|], TestName = "t26: Infection probability is 0%, Starting point 1, 1 move")>]
[<TestCase(1, 1, 3, Result = [|0; 2; 0; 0; 0; 0; 0; 0;|], TestName = "t26: Infection probability is 0%, Starting point 1, 3 moves")>]
[<TestCase(1, 4, 100, Result = [|0; 0; 0; 0; 2; 0; 0; 0;|], TestName = "t26: Infection probability is 0%, Starting point 4, 100 moves")>]
[<TestCase(1, 6, 100, Result = [|0; 0; 0; 0; 0; 0; 2; 0;|], TestName = "t26: Infection probability is 0%, Starting point 6, 100 moves")>]
let netTest (randVal, start, moves) =
  let mutable net = new Net ([(0, 1); (1, 2); (2, 3); (2, 4); (3, 5); (5, 4); (1, 6); (6, 4); (6, 7)], 
                             8, [|"Windows"; "Linux"; "OS X"; "Linux"; "Unknown"; "Windows"; "Windows"; "OS X"|])
  ((net :> IGraphMarked<Computer>).ValAt start).ChangeInfection 2 
  for i = 0 to moves - 1 do
    net.emulateNextStep randVal
  net.InfectionToArray

(*
   Windows    Linux         OS X
*)
[<TestCase(0, 1, 1, Result = [|0; 2; 0|],
    TestName = "t26: Infection probability is 100%, graph with no verges, 1 move from node 1")>]
[<TestCase(0, 1, 100, Result = [|0; 2; 0|],
    TestName = "t26: Infection probability is 100%, graph with no verges, 100 moves from node 1")>]
[<TestCase(0, 0, 100, Result = [|2; 0; 0|],
    TestName = "t26: Infection probability is 100%, graph with no verges, 100 moves from node 0")>]
let netTest2 (randVal, start, moves) =
  let mutable net = new Net ([], 3, [|"Windows"; "Linux"; "OS X"|])
  ((net :> IGraphMarked<Computer>).ValAt start).ChangeInfection 2 
  for i = 0 to moves - 1 do
    net.emulateNextStep randVal
  net.InfectionToArray

(*
    0.Unknown <---- 1. Unknown <---- 2.Unknown
                         /\
                          |
                          |
                     3. Unknown                         4. Windows -----> 5.Linux
*)
//In this tests two (or one) starting points will be tested
[<TestCase(0, 3, 3, 1, Result = [|0; 2; 0; 2; 0; 0|], 
        TestName = "t26: 2 connected components; Infection probability is 100%, Starting point 3, 1 move")>]

[<TestCase(0, 2, 3, 1, Result = [|0; 2; 2; 2; 0; 0|], 
        TestName = "t26: 2 connected components; Infection probability is 100%, Starting point 3 & 2, 1 move")>]

[<TestCase(1, 3, 3, 1, Result = [|0; 0; 0; 2; 0; 0|], 
        TestName = "t26: 2 connected components; Infection probability is 0%, Starting point 3, 1 move")>]

[<TestCase(1, 2, 3, 1, Result = [|0; 0; 2; 2; 0; 0|], 
        TestName = "t26: 2 connected components; Infection probability is 0%, Starting point 3 & 2, 1 move")>]

[<TestCase(0, 3, 3, 100, Result = [|2; 2; 0; 2; 0; 0|], 
        TestName = "t26: 2 connected components; Infection probability is 100%, Starting point 3, 100 moves")>]

[<TestCase(0, 2, 3, 100, Result = [|2; 2; 2; 2; 0; 0|], 
        TestName = "t26: 2 connected components; Infection probability is 100%, Starting point 3 & 2, 100 moves")>]

[<TestCase(1, 3, 3, 100, Result = [|0; 0; 0; 2; 0; 0|], 
        TestName = "t26: 2 connected components; Infection probability is 0%, Starting point 3, 100 moves")>]

[<TestCase(1, 2, 3, 100, Result = [|0; 0; 2; 2; 0; 0|], 
        TestName = "t26: 2 connected components; Infection probability is 0%, Starting point 3 & 2, 100 moves")>]

[<TestCase(0, 4, 4, 100, Result = [|0; 0; 0; 0; 2; 2|], 
        TestName = "t26: 2 connected components; Infection probability is 100%, Starting point 4, 100 moves")>]

[<TestCase(1, 4, 4, 100, Result = [|0; 0; 0; 0; 2; 0|], 
        TestName = "t26: 2 connected components; Infection probability is 0%, Starting point 4, 100 moves")>]
let netTest3 (randVal, start, start2, moves) =
  let mutable net = new Net ([(1, 0); (2, 1); (3, 1); (4, 5)], 
                             6, [|"Unknown"; "Unknown"; "Unknown"; "Unknown"; "Windows"; "Linux"|])
  ((net :> IGraphMarked<Computer>).ValAt start2).ChangeInfection 2 
  ((net :> IGraphMarked<Computer>).ValAt start).ChangeInfection 2 
  for i = 0 to moves - 1 do
    net.emulateNextStep randVal
  net.InfectionToArray
(*[<EntryPoint>]
let main argv = 
  printfn "Controls: press any key for the next move; press 'Esc' to quit."
  let mutable net = new Net ([(0, 1); (1, 2); (2, 3); (2, 4); (3, 5); (5, 4); (1, 6); (6, 4); (6, 7)], 8, 
          [|"Windows"; "Linux"; "OS X"; "Linux"; "Unknown"; "Windows"; "Windows"; "OS X"|])
  ((net :> IGraphMarked<Computer>).ValAt 0).ChangeInfection 2 
  0*)