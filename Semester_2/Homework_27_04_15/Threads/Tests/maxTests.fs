module Tests
open NUnit.Framework
open Programs
[<TestCase (1, 1000000, Result = 2147483082, TestName = "ArMax: 1 thread")>]//0.121
[<TestCase (2, 1000000, Result = 2147483082, TestName = "ArMax: 2 threads")>]//0.122
[<TestCase (3, 1000000, Result = 2147483082, TestName = "ArMax: 3 threads")>]//0.099
[<TestCase (4, 1000000, Result = 2147483082, TestName = "ArMax: 4 threads")>]//0.124
[<TestCase (100, 1000000, Result = 2147483082, TestName = "ArMax: 33 threads")>]//0.111
let maxInArTest threadNumber arraySize : int =
  let rnd = System.Random (0)
  let arr = Array.init arraySize (fun _ -> rnd.Next())
  let res = maxInAr threadNumber arr
  res
