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
          let threadRes = maxInRange arr (i * step) ((i+1) * step - 1)
          lock res (fun _ -> res := max res.Value threadRes)
        ))
    )

  let threadRes = maxInRange arr ((threadNumber - 1) * step) (arr.Length - 1)
  res := max res.Value threadRes

  for t in threadArray do
    t.Start()
    
  for t in threadArray do
    t.Join()
  res.Value

let arMulSum (a : int[,]) (b : int[,]) row col =
  let mutable res = 0
  let comLen = min (a.GetLength 1) (b.GetLength 0)
        //these lengths should be the same, but this line will help in case they are not.
  for i in 0.. comLen - 1 do
    res <- res + a.[row, i] * b.[i, col] 
  res

let matrMul threadNumber (a : int[,]) (b : int[,]) = 
  let mutable threadNumber = threadNumber
  let alenR = (a.GetLength 0) //Rows
  let alenC = (a.GetLength 1) //Columns
  let blenC = (b.GetLength 1)
  let blenR = (b.GetLength 0)
  let res = (Array2D.zeroCreate alenR blenC)
  let step = alenR / threadNumber
  if (step = 0) then threadNumber <- alenC

  let mulRows begR endR = 
    for k in begR .. endR do
      for j in 0 .. blenC - 1 do
        res.[k, j] <- arMulSum a b k j

  let mutable threadArray = Array.init (threadNumber - 1) (fun i ->
      new Thread(ThreadStart(fun _ -> mulRows (i * step) ((i + 1) * step - 1)))
    )

  mulRows  ((threadNumber - 1) * step) (alenR - 1)

  for t in threadArray do
    t.Start()
    
  for t in threadArray do
    t.Join()
  res

let integralCalcRange (f : double -> double) l r h =
  let delta = 0.000001
  let mutable res : double = 0.0
  for i in l .. h .. (r - h + delta) do 
      //sometimes I get something like 1.200000000000002 instead of proper 1.2, and that's why I add `delta`
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
          let li = (l + i * step * h)
          let ri = (l + (i + 1.0) * step* h)
          let threadRes = integralCalcRange f li ri h
          lock res (fun _ -> res.Value <- res.Value + threadRes)
        ))
    )

  let threadRes = integralCalcRange f (l + double (threadNumber - 1) * step * h) r h
  res.Value <- res.Value + threadRes

  for t in threadArray do
    t.Start()
    
  for t in threadArray do
    t.Join()
  res.Value
  
[<EntryPoint>]
let main argv = 
  0
