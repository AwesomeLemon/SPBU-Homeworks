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

let maxInArTest threadNumber arraySize : int =
  let rnd = System.Random (0)
  let arr = Array.init arraySize (fun _ -> rnd.Next())
  maxInAr threadNumber arr

let arMulSum (a : int[]) (b : int[]) =
  let mutable res = 0
  let comLen = min a.Length b.Length
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
  let alenR = (a.GetLength 1)
  let alenC = (a.GetLength 0)
  let blenC = (b.GetLength 1)
  let blenR = (b.GetLength 0)
  let res = ref (Array2D.zeroCreate alenC blenC)
  let step = alenR / threadNumber
  if (step = 0) then threadNumber <- alenR
  let mutable threadArray = Array.init (threadNumber - 1) (fun i ->
      
      new Thread(ThreadStart(fun _ ->
          Monitor.Enter(res)
          for k in (i * step) .. ((i + 1) * step - 1) do
            for j in 0 .. blenC - 1 do
              res.Value.[k, j] <- arMulSum (getRow a k) (getColumn b j)
          Monitor.Exit(res)
        ))
      
    )
  let threadNumber = threadNumber
  threadArray <- Array.append threadArray [| new Thread(ThreadStart(fun _ ->
          Monitor.Enter(res)
          for k in ((threadNumber - 1) * step) .. (alenC - 1) do
            for j in 0 .. blenC - 1 do
              res.Value.[k, j] <- arMulSum (getRow a k) (getColumn b j)
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
 // maxInArTest 40 10  |> printfn "%A"
  let a = array2D [ [1; 2; 3]; [1; 2; 3]; ]
  let b = array2D [ [1; 17]; [1; 2]; [1; 2] ]
 // let c = arMulSum [|1; 2; 3|] [|-1; 17; 3|] |> printfn "%A"
 // getRow a 0 |>printfn "%A"
 // getColumn a 1 |>printfn "%A"
  let c = matrMul 1 a b |> printfn "%A"
  0
