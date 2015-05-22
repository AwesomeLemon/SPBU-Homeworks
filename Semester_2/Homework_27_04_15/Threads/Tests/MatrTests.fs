module MatrTests
open Programs
open NUnit.Framework

[<TestCase (1, 10, TestName = "Matr: 1 thread, size 10")>]//0.063
[<TestCase (1, 3000, TestName = "Matr: 1 thread, size 3000")>]//1.627
[<TestCase (2, 3000, TestName = "Matr: 2 threads, size 3000")>]//1.679
[<TestCase (4, 3000, TestName = "Matr: 4 threads, size 3000")>]//1.033
[<TestCase (1000, 3000, TestName = "Matr: 100 threads, size 3000")>]//2.342
let matrMulTest threadNumber size =
  let a = Array2D.init 100 size (fun i j -> 2)
  let b = Array2D.init size 100 (fun i j -> 3)
  let res = matrMul threadNumber a b
  Assert.AreEqual (res, Array2D.init 100 100 (fun i j-> 6 * size))
