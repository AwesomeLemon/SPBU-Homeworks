module Tests
open simpleCalc
open NUnit.Framework

let Action (c : char) = 
  nooAction (nooButton c 0 0)

[<TestCase ("123", "-20", '-', Result = "143", TestName = "Subtracting negative number")>]
[<TestCase ("-2", "-3", '*', Result = "6", TestName = "Multiplying negative numbers")>]
[<TestCase ("-1", "1", '+', Result = "0", TestName = "Adding")>]
[<TestCase ("-9", "4", '/', Result = "-2", TestName = "Dividing negative number by positive")>]
[<TestCase ("5", "0", '/', Result = "Don't divide by zero", TestName = "Dividing by zero")>]
let test (s1 : string) (s2 : string) (op : char) =
  for i = 0 to s1.Length - 1 do
    Action s1.[i]
  Action op
  for i = 0 to s2.Length - 1 do
    Action s2.[i]
  Action '='
  cl.Clean ()
  programLabel.Text

[<TestCase ("1", "-3", "2", '-', '*', Result = "8", TestName = "Two actions")>]
[<TestCase ("19", "6", "0", '/', '+', Result = "3", TestName = "Two other actions")>]
let longTest (s1 : string) (s2 : string) (s3 : string) (op1 : char) (op2 : char) =
  for i = 0 to s1.Length - 1 do
    Action s1.[i]
  Action op1
  for i = 0 to s2.Length - 1 do
    Action s2.[i]
  Action '='
  Action op2
  for i = 0 to s3.Length - 1 do
    Action s3.[i]
  Action '='
  cl.Clean ()
  programLabel.Text