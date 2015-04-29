module IntegralTests
open Programs
open NUnit.Framework

[<TestCase(1, -3.1415, 3.1415, Result = 0.000, TestName = "Integral: 1 thread, sin x")>]
[<TestCase(4, -3.1415, 3.1415, Result = 0.000, TestName = "Integral: 4 threads, sin x")>]
let IntTest1 threadNumber l r =
  let cutNumber = 100
  let f = (fun x -> sin x )
  let res = integralCalc threadNumber f l r cutNumber
  System.Math.Round (res * 1000.0) / 1000.0


[<TestCase(1, 0.0, 3.0, Result = 9.000, TestName = "Integral: 1 thread, x ^ 2")>]
[<TestCase(4, -3.0, 0.0, Result = 9.000, TestName = "Integral: 4 threads, x ^ 2")>]
let IntTest2 threadNumber l r =
  let cutNumber = 100
  let f = (fun x -> x * x )
  let res = integralCalc threadNumber f l r cutNumber
  System.Math.Round (res * 1000.0) / 1000.0

[<TestCase(2, 0.0, 3.0, Result = 3.000, TestName = "Integral: 2 threads, y = 1")>]
[<TestCase(100, -3.0, 0.0, Result = 3.000, TestName = "Integral: 100 threads, y = 1")>]
let IntTest3 threadNumber l r =
  let cutNumber = 100
  let f = (fun x -> 1.0 )
  let res = integralCalc threadNumber f l r cutNumber
  System.Math.Round (res * 1000.0) / 1000.0