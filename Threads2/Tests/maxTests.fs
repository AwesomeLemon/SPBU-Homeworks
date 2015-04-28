module Tests
open NUnit.Framework
open Programs
[<TestCase (1, 1000000, Result = 2147483082, TestName = "One thread")>]
[<TestCase (2, 1000000, Result = 2147483082, TestName = "Two threads")>]
[<TestCase (3, 1000000, Result = 2147483082, TestName = "Three threads")>]
[<TestCase (4, 1000000, Result = 2147483082, TestName = "Four threads")>]
[<TestCase (33, 1000000, Result = 2147483082, TestName = "33 threads")>]
let maxInArTest threadNumber arraySize : int =
  let rnd = System.Random (0)
  let arr = Array.init arraySize (fun _ -> rnd.Next())
  let res = maxInAr threadNumber arr
  res

let matrMulTest threadNumber size =
  let a = Array2D.init 10 size (fun i j-> i + j)
  let b = Array2D.init size 10 (fun i j-> i + j)
  let res = matrMul threadNumber a b
  res