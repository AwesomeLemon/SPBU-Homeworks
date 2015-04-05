//Tasks 37, 38: to OPN ; Stack calculator
//       by Alexander Chebykin
// Expected time: 3 hrs
// Actual time: 4.5 hrs
open System
open NUnit.Framework
exception Error of string
exception ErrorWithParam of string * string
type CalcTree = Nil | Oper of char * CalcTree * CalcTree | Data of int
type stack<'A> () = 
    class
        let mutable st = [] : 'A list
        member self.Self = st
        member self.Push data = 
            st <- data :: st
        member self.Pop =
            match st with
            | [] -> 
                raise(Error( "Pop: Empty Stack"))
                    //In the current state of code this error will never be raised, but let it be.
            | x :: l ->
                st <- l
        member self.Top =
            match st with
            | [] -> 
                raise (Error ( "Top: Empty Stack"))
                    //In the current state of code this error will never be raised, but let it be.
            | x :: l ->
                x
        member self.IsEmpty = 
            match st with
            | [] -> true
            | _ -> false
        member self.Print =
            let rec print vals = 
                match vals with
                | [] -> printfn ""
                | x :: l -> 
                    printf "%A " x
                    print l
            
            print st
    end
let expressionToOPN (str : string) =
    let mutable i = 0
    let operStack = new stack<(char * int)> ()
    let mutable out = ""
    let operations = [|('*', 3); ('/', 3); ('%', 3); ('-', 2); ('+', 2); ('^', 4);|]

    while (i < str.Length) do 
        match str.[i] with
        | '(' -> 
            operStack.Push ('(', -1)
            i <- i + 1
            let mutable mayBeNumberFlag = true
            let mutable isScanningOverFlag = false
            let mutable res = ""
            while (i < str.Length) && mayBeNumberFlag && not isScanningOverFlag do
                match str.[i] with
                | ' ' -> i <- i + 1
                | '-' ->
                    i <- i + 1
                    
                    while (i < str.Length) && not isScanningOverFlag do
                        match str.[i] with
                        | ' ' -> i <- i + 1
                        | _ -> 
                            if (Char.IsDigit(str.[i])) then
                               while (i < str.Length) && (Char.IsDigit(str.[i])) do
                                       res <- res + str.[i].ToString()
                                       i <- i + 1
                               isScanningOverFlag <- true
                            else
                                raise (Error ("There can't be two operators in a row"))
                | _ -> mayBeNumberFlag <- false
            if (mayBeNumberFlag) then out <- out + "-" + res + "\r\n"    
        | ')' ->
            try
                let mutable topValue = fst operStack.Top
                while not (topValue = '(') do
                    out <- out + topValue.ToString() + "\r\n"
                    operStack.Pop
                    topValue <- fst operStack.Top
                if (topValue = '(') then
                    operStack.Pop
                else raise (Error ("Mismatching parenthesis"))
            with
            |Error(msg) -> raise (Error ("Mismatching parenthesis"))
            i <- i + 1
        | ' ' -> i <- i + 1
        | _ -> 
            if (Char.IsDigit(str.[i])) then 
                let mutable buf = ""
                while (i < str.Length) && (Char.IsDigit(str.[i])) do
                    buf <- buf + str.[i].ToString()
                    i <- i + 1
                out <- out + buf + "\r\n"
            else
                let j = i
                let isOperator elem =  
                    match elem with
                    | (a, _) -> 
                      str.[j] = a

                if (Array.exists isOperator operations) then
                    let a = Array.findIndex isOperator operations
                    if (a >= 0) && (a < operations.Length) then 
                        if (operStack.IsEmpty) then
                            operStack.Push operations.[a]
                            i <- i + 1
                        else
                            let mutable topPrecedence = snd operStack.Top
                            let mutable topValue = fst operStack.Top
                            let curPrecedence = snd operations.[a]
                            let mutable eval = (<=)
                            if (curPrecedence = 4) then eval <- (<)
                            let mutable stackIsEmptyFlag = false
                            //I've tried to separate 'while' condition into function, 
                            //but F# said that this way I'm trying to make mutable variables into constant,
                            //and it can't allow it
                            while topValue <> '(' && 
                                        (eval curPrecedence topPrecedence) &&
                                                            not stackIsEmptyFlag do
                                out <- out + topValue.ToString() + "\r\n"

                                operStack.Pop
                                if not (operStack.IsEmpty) then 
                                    topPrecedence <- snd operStack.Top
                                    topValue <- fst operStack.Top
                                else stackIsEmptyFlag <- true
                            operStack.Push operations.[a]
                            i <- i + 1
                  else raise (Error ("Unsupported symbol in the input"))
    while not (operStack.IsEmpty) do
        let cur = fst operStack.Top
        out <- out + cur.ToString() + "\r\n"

        operStack.Pop
    out

let fileToOPN (fIn: string) (fOut : string)= 
  use input = new IO.StreamReader (fIn)
  let s = input.ReadToEnd ()
  let res = try 
                expressionToOPN s
            with
            | Error(msg) -> ("Error: " + msg)
            | ErrorWithParam(msg, param) -> ("Error: " + msg + param)
  use output = new IO.StreamWriter (fOut)
  output.Write (res)


let rec calculateTree tree = 
    match tree with
    | Data (x) -> x
    | Nil -> raise (Error ("The tree is empty, which is wrong")) 
                   //In the current state of code this error will never be raised, but let it be.
    | Oper (x, l, r) ->
        let oper =  match x with
                    | '*' -> (*)
                    | '/' -> (/)
                    | '-' -> (-)
                    | '+' -> (+)
                    | '%' -> (%)
                    | '^' -> (pown)
                    | _ -> raise (ErrorWithParam ("Unknown operator: ", x.ToString()))
                            //In the current state of code this error will never be raised, but let it be.
        let a = oper (calculateTree l) (calculateTree r)
        a

let fileToTree (fIn : string) =
    let out = new stack<CalcTree> ()
    let mutable buf = ""
    let uniteIntoBiggerTree operatorValue=
      try  
        let temp = out.Top
        out.Pop
        let curRes = Oper (operatorValue, out.Top, temp)
        out.Pop
        out.Push curRes
      with
      | Error(msg) -> raise(Error ("Not enough integer data"))
    let isOperator x =
      match x with
        | '*' -> true
        | '/' -> true
        | '-' -> true
        | '+' -> true
        | '%' -> true
        | '^' -> true
        | _ -> false
    let stringPartToInt (buf : string) start = 
        let mutable start = start
        let mutable res = ""
        while ((start < buf.Length) && ((Char.IsDigit(buf.[start])))) do 
            res <- res + buf.[start].ToString()
            start <- start + 1
        let res = int res//it's guaranteed that string 'res' contains only digits
        res
    
    use input = new IO.StreamReader (fIn)
    buf <- input.ReadLine ()

    if (buf.Length > 6) then
      if (buf.[0..6] = "Error: ") then //Error has occured while converting to OPN
        raise (Error(buf))

    while (buf <> null) do
      if (isOperator buf.[0]) then
        if (buf.[0] = '-') && (buf.Length > 1) then
          if Char.IsDigit(buf.[1]) then
            out.Push (Data(- (stringPartToInt buf 1)))
          else
            raise (Error("Bad input"))
        else
          uniteIntoBiggerTree buf.[0]
      else
        if Char.IsDigit(buf.[0]) then
          out.Push (Data (stringPartToInt buf 0))
        else
          raise (Error("Bad input"))
      buf <- input.ReadLine ()
    out.Top

let calculateOPNFile (fIn : string) (fOut : string)= 
    let res = try
                (calculateTree (fileToTree(fIn))).ToString()
              with
              | Error(msg) -> msg
              | ErrorWithParam(msg, param) -> (msg + param)
    use output = new IO.StreamWriter (fOut)
    output.Write (res)


[<TestCase ("in0.txt", "opn0.txt", "res0.txt", "1 -2 - 3", TestName = "Usual 0")>]
[<TestCase ("in1.txt", "opn1.txt", "res1.txt", "1 + (2 / 3) ^2", TestName = "Usual 1")>]
[<TestCase ("in2.txt", "opn2.txt", "res2.txt", "17 - 456 * 11", TestName = "Usual 2")>]
[<TestCase ("in3.txt", "opn3.txt", "res3.txt", "(34 + 81) * 59 / 134 - (35 - 31) ^ 3", TestName = "Usual 3")>]
[<TestCase ("in4.txt", "opn4.txt", "res4.txt", "(98 /3)*(-4)^2 + ((-64)%  5) ^ 3", TestName = "Usual 4")>]
[<TestCase ("in5.txt", "opn5.txt", "res5.txt", "(-19) * ((- +2) / 3) ^2", TestName = "Two operators in a row")>]
[<TestCase ("in6.txt", "opn6.txt", "res6.txt", "1 + 2 / 3) ^2", TestName = "Mismatching parenthesis// no '('")>]
[<TestCase ("in7.txt", "opn7.txt", "res7.txt", ")(98 /3)*(-4)^2 + ((-64)-  5) ^ 3", 
      TestName = "Mismatching parenthesis// ')' as the first symbol")>]
[<TestCase ("in8.txt", "opn8.txt", "res8.txt", "(98 /3)*(-4)^2 + ((-64)&  5) ^ 3", 
      TestName = "Unsupported symbol in the input")>]
[<TestCase ("in9.txt", "opn9.txt", "res9.txt", "3 ^ 1 ^ 2", TestName = "Right association of '^' ")>]

let testTask37 (fIn : string) (fOut : string)(fRes : string) (expr : string) = 
  let input = new IO.StreamWriter (fIn)
  input.Write (expr)
  input.Dispose()

  fileToOPN fIn fOut

  use output = new IO.StreamReader (fOut)
  let res = output.ReadToEnd ()
  use trueOut = new IO.StreamReader (fRes)
  let trueRes = trueOut.ReadToEnd ()
  Assert.AreEqual (res, trueRes)

//The expressions are opn representations of expressions in tests for task 37
[<TestCase ("opn0.txt", "data0.txt", "-4", TestName = "T38: Usual 0")>]
[<TestCase ("opn1.txt", "data1.txt", "1", TestName = "T38: Usual 1")>]
[<TestCase ("opn2.txt", "data2.txt", "-4999", TestName = "T38: Usual 2")>]
[<TestCase ("opn3.txt", "data3.txt", "-14", TestName = "T38: Usual 3")>]
[<TestCase ("opn4.txt", "data4.txt", "448", TestName = "T38: Usual 4")>]
[<TestCase ("opn5.txt", "data5.txt", "Error: There can't be two operators in a row", 
      TestName = "T38: Two operators in a row")>]
[<TestCase ("opn6.txt", "data6.txt", "Error: Mismatching parenthesis", 
      TestName = "T38: Mismatching parenthesis// no '('")>]
[<TestCase ("opn7.txt", "data7.txt", "Error: Mismatching parenthesis", 
      TestName = "T38: Mismatching parenthesis// ')' as the first symbol")>]
[<TestCase ("opn8.txt", "data8.txt", "Error: Unsupported symbol in the input", 
      TestName = "T38: Unsupported symbol in the input")>]
[<TestCase ("opn9.txt", "data9.txt", "3", TestName = "T38: Right association of '^'")>]

let testTask38 (fIn : string) (fOut : string) (trueRes : string) = 
  calculateOPNFile fIn fOut
  use output = new IO.StreamReader (fOut)
  let res = output.ReadToEnd ()
  Assert.AreEqual (res, trueRes)

[<EntryPoint>]
let main argv = 
  0
