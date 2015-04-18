module RingTests
open RingWorkflow
open NUnit.Framework
[<TestCase(5, 0,TestName = "Arithmetics 1")>]
[<TestCase(4, 2,TestName = "Arithmetics 2")>]
let calcRing n trueRes=
  let res = RingBuilder n {
    let! a = 2 * 3
    let! b = 4
    return (a + b)
  }
  Assert.AreEqual (res, Some trueRes)

[<TestCase(2, 0, TestName = "Arithmetics 3")>]
[<TestCase(12, 2, TestName = "Arithmetics 4")>]
let calcRing2 n trueRes=
  let res = RingBuilder n {
    let! a = -2 + -1 * 3
    let! b = 7 % 5
    return (a * b)
  }
  Assert.AreEqual (res, Some trueRes)

[<Test>]
let ``Combine and division by zero`` =
  let res = RingBuilder 20 {
    return 17
    return! 1 / 0
  }
  Assert.AreEqual (res, Some 17)

[<Test>]
let ``Combine`` =
  let res = RingBuilder 4 {
    return 7
    return (3 - 1)
  }
  Assert.AreEqual (res, Some 1)
