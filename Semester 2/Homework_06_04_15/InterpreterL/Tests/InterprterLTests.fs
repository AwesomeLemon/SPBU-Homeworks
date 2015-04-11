module Tests
open Interpreter
open Expression
open Parser
open NUnit.Framework

//TestCase doesn't want any list or empty array. 
//So when array should be empty I'll be passing '-12345'
[<TestCase ("in0.txt", [|-12345|], Result = [|"Some command in the input program is missing"|], 
      TestName = "No else in if")>]

[<TestCase ("in1.txt", [|-12345|], Result = [||], TestName = "While with false condition")>]
[<TestCase ("in2.txt", [|-12345|], Result = [|"3"; "2"|], TestName = "While inside of while")>]
[<TestCase ("in3.txt", [|0|], Result = [|"-1"|], 
      TestName = "Reading variable then overriding its' value with assigning")>]

[<TestCase ("in4.txt", [|1|], Result = [|"1"|], 
      TestName = "Assigning value to a variable then overriding it with reading")>]

[<TestCase ("in5.txt", [|1|], Result = [|"1"|], TestName = "Simple read - write")>]
[<TestCase ("in6.txt", [|-12345|], Result = [|"1"|], TestName = "Testing else")>]
[<TestCase ("in7.txt", [|-12345|], Result = [|"Unknown variable is being used: x"|], 
      TestName = "Writing variable without initiation")>]

[<TestCase ("in8.txt", [|-12345|], Result = [|"Number is incorrect: 1q"|], 
      TestName = "Incorrect number in the input")>]

[<TestCase ("in9.txt", [|-12345|], Result = [|"It is not digit that stands after minus"|], 
      TestName = "Incorrect number in the input")>]

[<TestCase ("in10.txt", [|-12345|], Result = [|"Unknown command: for"|],
      TestName = "Unknown command: for")>]

[<TestCase ("in11.txt", [|2; -3; 4|], Result = [|"-24"; "3"; "1"|],
      TestName = "Big input and output")>]

[<TestCase ("pown.txt", [|10; 2|], Result = [|"100"|], TestName = "Overall test")>]
[<TestCase ("pown.txt", [|-2; 9; 15|], Result = [|"-512"|], 
      TestName = "Overall test; excesive input")>]

[<TestCase ("pown.txt", [|-2|], Result = [|"Not enough variables values"|], 
      TestName = "Overall test; insufficient input")>]

let test (program : string) (input : int[]) = 
  let input  = new ListInput (Array.toList input) :> IInputProvider
  let output = new ListOutput () :> IOutputProvider
  try
    interprete (toTree program) input output
  with
  | Error (msg) -> output.Write (msg)
  | ErrorWithParam (msg, param) -> output.Write (msg + param)
  List.toArray ((output :?> ListOutput).Out ())