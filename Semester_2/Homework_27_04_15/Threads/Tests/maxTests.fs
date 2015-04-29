module Tests
open NUnit.Framework
open Programs
[<TestCase (1, 1000000, Result = 2147483082, TestName = "ArMax: 1 thread")>]
[<TestCase (2, 1000000, Result = 2147483082, TestName = "ArMax: 2 threads")>]
[<TestCase (3, 1000000, Result = 2147483082, TestName = "ArMax: 3 threads")>]
[<TestCase (4, 1000000, Result = 2147483082, TestName = "ArMax: 4 threads")>]
[<TestCase (33, 1000000, Result = 2147483082, TestName = "ArMax: 33 threads")>]
let maxInArTest threadNumber arraySize : int =
  let rnd = System.Random (0)
  let arr = Array.init arraySize (fun _ -> rnd.Next())
  let res = maxInAr threadNumber arr
  res
