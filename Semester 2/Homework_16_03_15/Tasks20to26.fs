//Different graph interfaces and implementations
//                by Alexander Chebykin
open System
//Task 20: graph interface
type IGraph<'A>  =
  interface
    abstract IsWay : int -> int -> bool
    abstract Size  : int
    abstract ValAt : int -> 'A
  end
//Task 25: polymorph graph interface
type IGraphMarked<'A>  =
  interface
    inherit IGraph<'A>
    abstract WayLength  : int -> int -> int
    abstract SetLengths : int[] -> unit
  end
//Task 21: IGraph implementation with adjacency matrix
type ArrayOrgraph<'A> (verges: list<int*int>, numberOfNods, vals: 'A[]) =
  class
    let nodeVals = vals
    let adjMatrix = Array2D.zeroCreate numberOfNods numberOfNods
    do 
      for (n1, n2) in verges do Array2D.set adjMatrix n1 n2 1
    interface IGraph<'A> with
      override s.IsWay a b =
        if (adjMatrix.[a, b] = 1) then true else false
      override s.Size = 
        Array2D.length1 adjMatrix
      override s.ValAt a = 
        nodeVals.[a]
    end
  end
//Task 21: IGraph implementation with adjacency lists
type ListOrgraph<'A> (verges: list<int*int>, numberOfNods, vals: 'A[]) = 
  class 
    let nodeVals = vals
    let nodes = Array.create numberOfNods []
    do 
      for (n1, n2) in verges do nodes.[n1] <- n2 :: nodes.[n1]
    interface IGraph<'A> with
      override s.IsWay a b =
        let rec listFind x l = 
          match l with
          | [] -> None
          | a :: l -> 
            if (x = a) then Some x
               else listFind x l
        if (listFind b (nodes.[a]) = Some b) then true else false

      override s.Size =
        nodes.GetLength 0
      override s.ValAt a = 
        nodeVals.[a]
    end
  end
//Task 23
let canGoTo graph init = 
  let visited = Array.create (graph :> IGraph<'A>).Size false
  visited.[init] <- true
  let rec find (visited :bool[]) gr cur = 
    let mutable res = []
    for i = 0 to ((gr :> IGraph<'A>).Size - 1) do
      if ((gr :> IGraph<'A>).IsWay cur i) then 
        if (visited.[i] = false) then 
          visited.[i] <- true
          res <- i :: res @ (find visited gr i)
    res
  List.sort (find visited graph init)
//Task 24
let canGetFrom graph init = 

  let rec find (visited :bool[]) gr cur target = 
    let mutable res = false
    for i = 0 to ((gr :> IGraph<'A>).Size - 1) do
      if ((gr :> IGraph<'A>).IsWay cur i) then 
        if (visited.[i] = false) then 
          if (i = target) then res <- true
          else
            visited.[i] <- true
            res <- res || (find visited gr i target)
    res

  let mutable res = []
  for i = 0 to ((graph :> IGraph<'A>).Size - 1) do
    if not (i = init) then
       let visited = Array.create (graph :> IGraph<'A>).Size false
       visited.[i] <- true
       if (find visited graph i init) then res <- i :: res
  List.sort res

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

type Net(verges: list<int*int>, numberOfNods, OSs :string[]) = 
  class
    inherit ArrayOrgraph<Computer> (verges, numberOfNods, [| for i in 0 .. OSs.Length - 1 -> new Computer(OSs.[i]) |])
    member self.PrintInfection = 
      for i = 0 to numberOfNods - 1 do
        printf "%i " ((self  :> IGraph<Computer>).ValAt i).Infection
      printfn ""
  end

let emulate (graph: Net) = 
  let random = System.Random()
  for i = 0 to (graph :> IGraph<Computer>).Size - 1 do
    if ((graph :> IGraph<Computer>).ValAt i).Infection = 2 then 
      for j = 0 to (graph :> IGraph<Computer>).Size - 1 do
        if (((graph :> IGraph<Computer>).IsWay i j) && 
            (((graph :> IGraph<Computer>).ValAt j).Infection = 0) &&
              (random.NextDouble() < ((graph :> IGraph<Computer>).ValAt j).InfectionChanse)) 
                then ((graph :> IGraph<Computer>).ValAt j).ChangeInfection 1
  // And now infected Computers are getting completely viral so that on the next mobe they'll be able to infect their neighbours:
  for i = 0 to (graph :> IGraph<Computer>).Size - 1 do
   if ((graph :> IGraph<Computer>).ValAt i).Infection = 1 then ((graph :> IGraph<Computer>).ValAt i).ChangeInfection 2
  graph 

[<EntryPoint>]
let main argv = 
  let gr = new ArrayOrgraph<int> ([(0, 1); (1, 2); (2, 3); (2, 4); (3, 4); (1, 5); (1, 6); (5, 7); (4, 5)], 8, [|for i in 0..8 -> i|])
  let gr2 = new ListOrgraph<int> ([(0, 1); (1, 2); (2, 3); (2, 4); (3, 4); (1, 5); (1, 6); (5, 7); (4, 5)], 8, [|for i in 0..8 -> i|])
  //Task 23
  let testNode = 3
  printfn "You can get from node %i to nodes %A//Matrix implementation" testNode (canGoTo gr testNode)
  printfn "You can get from node %i to nodes %A//Lists implementation" testNode (canGoTo gr2 testNode)
  //Task 24
  printfn "You can get to node %i from nodes %A//Matrix implementation" testNode (canGetFrom gr testNode)
  printfn "You can get to node %i from nodes %A//Lists implementation" testNode (canGetFrom gr2 testNode)
  //Task 26
  printfn "Controls: press any key for the next move; press 'Esc' to quit.\nNotation: 2 - viral computer; 0 - healthy"
  let mutable net = new Net ([(0, 1); (1, 2); (2, 3); (2, 4); (3, 4); (1, 5); (1, 6); (5, 7); (4, 5)], 8, [|"Windows"; "Linux"; "OS X"; "Linux"; "Unknown"; "Windows"; "Windows"; "OS X"|])
  ((net :> IGraph<Computer>).ValAt 0).ChangeInfection 2 
  let mutable flag = true
  while flag do
    match Console.ReadKey().Key with 
    | ConsoleKey.Escape -> flag <- false
    | _ -> 
      net <- emulate net
      net.PrintInfection
  0 
