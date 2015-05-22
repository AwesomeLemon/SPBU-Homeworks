module IntegralTests
//In these tests run time is small, so I'll write it in mili seconds
open Programs
open NUnit.Framework

[<TestCase(1, -3.1415, 3.1415, Result = 0.000, TestName = "Integral: 1 thread, sin x")>]//41,3 ms
[<TestCase(4, -3.1415, 3.1415, Result = 0.000, TestName = "Integral: 4 threads, sin x")>]//8,4818 ms
let IntTest1 threadNumber l r =
  let cutNumber = 100
  let f = (fun x -> sin x )
  let res = integralCalc threadNumber f l r cutNumber
  System.Math.Round (res * 1000.0) / 1000.0


[<TestCase(1, 0.0, 3.0, Result = 9.000, TestName = "Integral: 1 thread, x ^ 2")>]//2,2011 ms
[<TestCase(4, -3.0, 0.0, Result = 9.000, TestName = "Integral: 4 threads, x ^ 2")>]//1,0861 ms
let IntTest2 threadNumber l r =
  let cutNumber = 100
  let f = (fun x -> x * x )
  let res = integralCalc threadNumber f l r cutNumber
  System.Math.Round (res * 1000.0) / 1000.0

[<TestCase(2, 0.0, 3.0, Result = 3.000, TestName = "Integral: 2 threads, y = 1")>]//4,8297 ms
[<TestCase(100, -3.0, 0.0, Result = 3.000, TestName = "Integral: 100 threads, y = 1")>]//22,8138 ms
let IntTest3 threadNumber l r =
  let cutNumber = 100
  let f = (fun x -> 1.0 )
  let res = integralCalc threadNumber f l r cutNumber
  System.Math.Round (res * 1000.0) / 1000.0