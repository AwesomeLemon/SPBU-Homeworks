module Tests
open NUnit.Framework
open Calc
//TestCase doesn't like arrays of pairs. And empty arrays. 
//So when there're no variables, I'll be passing to it "unusedVariable" with value "-1", so it will work.
[<TestCase ("3 ^ 1 ^ 2", [|"unusedVariable"|], [|-1.0|],Result = "3", 
    TestName = "Right association of '^' ")>]

[<TestCase ("1 -2 - 3", [|"unusedVariable"|], [|-1.0|],Result = "-4", 
    TestName = "Expression without variables 0")>]

[<TestCase ("(20 + 80) * 5 / 2 - (35 - 31) ^ 3", [|"unusedVariable"|], [|-1.0|],Result = "186", 
    TestName = "Expression without variables 1")>]

[<TestCase ("(-19) * ((- +2) / 3) ^2", [|"unusedVariable"|], [|-1.0|],Result = "There can't be two operators in a row", 
    TestName = "Expression with two operators in a row")>]

[<TestCase ("1 + (2 / 3 ^2", [|"unusedVariable"|], [|-1.0|],Result = "Not enough float data", //'cause '(' will be considered
                                                                                             // as operator and will be put into tree, 
    TestName = "Expression with mismatching parenthesis //no ')'")>]                        //taking '1' for itself and leaving empty stack for '+'

[<TestCase ("1 + 2 / 3) ^2", [|"unusedVariable"|], [|-1.0|],Result = "Mismatching parenthesis", 
    TestName = "Expression with mismatching parenthesis //no '('")>]

[<TestCase (")(98 /3)*(-4)^2 + ((-64)-  5) ^ 3", [|"unusedVariable"|], [|-1.0|],Result = "Mismatching parenthesis", 
    TestName = "')' at the beginning")>]

[<TestCase ("(98 /3)*(-4)^2 + ((-64)&  5) ^ 3", [|"unusedVariable"|], [|-1.0|],Result = "Not enough operators", 
    TestName = "Expression without variables with wrong operator")>]

[<TestCase ("2.5 ^ 2 ^ boy", [|"boy"|], [|2.0|],Result = "39,0625", 
    TestName = "Expression with variable 1")>]

[<TestCase ("x / 2.5", [|"x"|], [|4.0|],Result = "1,6", 
    TestName = "Expression with variable 2")>]

let testFunc (str : string) (vars: string []) (varsVals : float[]) =
    let pars = [|for i in 0.. vars.Length - 1 -> (vars.[i], varsVals.[i])|]
    calculateString str pars