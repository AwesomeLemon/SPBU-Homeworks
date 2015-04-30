module Programs
open System.Threading
let maxInRange (arr : int []) l r : int =
  let mutable res = arr.[l]
  for i in l + 1 .. r do
    res <- max res arr.[i]
  res

let maxInAr threadNumber (arr : int[]) =
  let res = ref arr.[0]
  let step = arr.Length / threadNumber
  let mutable threadArray = Array.init (threadNumber - 1) (fun i ->
      new Thread(ThreadStart(fun _ ->
          Monitor.Enter(res)
          let threadRes = maxInRange arr (i * step) ((i+1) * step - 1)
          res := max res.Value threadRes
          Monitor.Exit(res)
        ))
    )
  threadArray <- Array.append threadArray  [|new Thread(ThreadStart(fun _ ->
          Monitor.Enter(res)
          let threadRes = maxInRange arr ((threadNumber - 1) * step) (arr.Length - 1)
          res := max res.Value threadRes
          Monitor.Exit(res)
        )) |]
  for t in threadArray do
    t.Start()
    
  for t in threadArray do
    t.Join()
  res.Value


let arMulSum (a : int[]) (b : int[]) =
  let mutable res = 0
  let comLen = min a.Length b.Length 
        //these lengths should be the same, but this line will help in case they are not.
  for i in 0.. comLen - 1 do
    res <- res + a.[i] * b.[i] 
  res

let getRow (a : int[,]) ind = 
  let mutable res = []
  let len = a.GetLength 1
  for i in 0..len - 1 do
    res <- a.[ind, i] :: res
  List.toArray (List.rev res)

let getColumn (a : int[,]) ind = 
  let mutable res = []
  let len = a.GetLength 0
  for i in 0..len - 1 do
    res <- a.[i, ind] :: res
  List.toArray (List.rev res)

let matrMul threadNumber (a : int[,]) (b : int[,]) = 
  let mutable threadNumber = threadNumber
  let alenR = (a.GetLength 0) //Rows
  let alenC = (a.GetLength 1) //Columns
  let blenC = (b.GetLength 1)
  let blenR = (b.GetLength 0)
  let res = (Array2D.zeroCreate alenR blenC)
  let step = alenR / threadNumber
  if (step = 0) then threadNumber <- alenC
  let mutable threadArray = Array.init (threadNumber - 1) (fun i ->
      
      new Thread(ThreadStart(fun _ ->
          for k in (i * step) .. ((i + 1) * step - 1) do
            for j in 0 .. blenC - 1 do
              res.[k, j] <- arMulSum (getRow a k) (getColumn b j)
        ))
      
    )
  let threadNumber = threadNumber //Otherwise there's closure
  threadArray <- Array.append threadArray [| new Thread(ThreadStart(fun _ ->
          Monitor.Enter(res)
          for k in ((threadNumber - 1) * step) .. (alenR - 1) do
            for j in 0 .. blenC - 1 do
              res.[k, j] <- arMulSum (getRow a k) (getColumn b j)
          Monitor.Exit(res)
        ))
        |]
  for t in threadArray do
    t.Start()
    
  for t in threadArray do
    t.Join()
  res

let integralCalcRange (f : double -> double) l r h =
  let mutable res : double = 0.0
  for i in l .. h .. (r - h + 0.000001) do 
      //sometimes I get something like 1.200000000000002 instead of proper 1.2, and that's why I add 0.000001
    res <- res + ((f i) + (f (i + h))) * h * 0.5
  res

let integralCalc threadNumber (f : double -> double) l r cutNumber =
  let n = cutNumber
  let h = (r - l) / (double n)
  let res = ref 0.0
  let step = max (double cutNumber / double threadNumber) (1.0) 
    //in case there're more cuts than threads
  let threadNumber = min threadNumber cutNumber
  let mutable threadArray = Array.init (threadNumber - 1) (fun i ->
      new Thread(ThreadStart(fun _ ->
          let i = double i
          Monitor.Enter(res)
          let li = (l + i * step * h)
          let ri = (l + (i + 1.0) * step* h)
          let threadRes = integralCalcRange f li ri h
          res.Value <- res.Value + threadRes
          Monitor.Exit(res)
        ))
    )
  threadArray <- Array.append threadArray [|new Thread(ThreadStart(fun _ ->
          Monitor.Enter(res)
          let threadRes = integralCalcRange f (l + double (threadNumber - 1) * step * h) r h
          res.Value <- res.Value + threadRes
          Monitor.Exit(res)
        ))
    |]
  for t in threadArray do
    t.Start()
    
  for t in threadArray do
    t.Join()
  res.Value
  
[<EntryPoint>]
let main argv = 
  0
