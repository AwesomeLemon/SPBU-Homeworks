//Integer calculator (tasks 35, 36)
//       by Alexander Chebykin
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

let expressionToTreeParam (str : string, pars: (string * int) []) =
    let mutable i = 0
    let operStack = new stack<(char * int)> ()
    let out = new stack<CalcTree> ()
    let operations = [|('*', 3); ('/', 3); ('%', 3); ('-', 2); ('+', 2); ('^', 4);|]

    let uniteIntoBiggerTree operatorValue=
        try  
            let temp = out.Top
            out.Pop
            let curRes = Oper (operatorValue, out.Top, temp)
            out.Pop
            out.Push curRes
        with
        | Error(msg) -> raise(Error ("Not enough integer data"))
    let stringPartToInt start = 
        let mutable start = start
        let mutable buf = ""
        while ((start < str.Length) && ((Char.IsDigit(str.[start])))) do 
            buf <- buf + str.[start].ToString()
            start <- start + 1
        let buf = int buf
        (buf, start)

    while (i < str.Length) do 
        match str.[i] with
        | '(' -> 
            operStack.Push ('(', -1)
            let mutable j = i + 1
            let mutable mayBeNumberFlag = true
            let mutable isScanningOverFlag = false
            let mutable res = 0
            while (j < str.Length) && mayBeNumberFlag && not isScanningOverFlag do
                match str.[j] with
                | ' ' -> j <- j + 1
                | '-' ->
                    j <- j + 1
                    
                    while (j < str.Length) && not isScanningOverFlag do
                        match str.[j] with
                        | ' ' -> j <- j + 1
                        | _ -> 
                            if (Char.IsDigit(str.[j])) then
                                let temp = stringPartToInt j
                                res <- fst temp
                                j <- snd temp
                                isScanningOverFlag <- true
                            else 
                                if (Char.IsLetter(str.[j])) then
                                   let mutable buf = ""
                                   let mutable noSuchVarFlag = true
                                   while (j < str.Length) && (Char.IsLetter(str.[j])) do
                                       buf <- buf + str.[j].ToString()
                                       j <- j + 1
                                   for k = 0 to pars.Length - 1 do
                                       if (fst pars.[k] = buf) then
                                         res <- snd pars.[k]
                                         noSuchVarFlag <- false
                                         isScanningOverFlag <- true
                                   if (noSuchVarFlag) then 
                                      raise(ErrorWithParam("There's no such variable as ", buf))
                                else
                                    raise (Error ("There can't be two operators in a row"))
                | _ -> mayBeNumberFlag <- false
            i <- j
            if (mayBeNumberFlag) then out.Push (Data (- res))    
        | ')' ->
            try
                let mutable topValue = fst operStack.Top
                while not (topValue = '(') do
                    uniteIntoBiggerTree topValue

                    operStack.Pop
                    topValue <- fst operStack.Top
                if (topValue = '(') then
                    operStack.Pop
                else raise (Error ("Mismatching parenthesis"))
            with
            |Error(msg) -> raise (Error ("Mismatching parenthesis"))
            i <- i + 1
        | _ -> 
            if (Char.IsDigit(str.[i])) then 
                let temp = stringPartToInt i
                out.Push (Data (fst temp))
                i <- snd temp
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
                            //but F# said that this way I'm trying to make mutable variables into constant
                            //, and it can't allow it
                            while topValue <> '(' && 
                                        (eval curPrecedence topPrecedence) &&
                                                            not stackIsEmptyFlag do
                                uniteIntoBiggerTree topValue

                                operStack.Pop
                                if not (operStack.IsEmpty) then 
                                    topPrecedence <- snd operStack.Top
                                    topValue <- fst operStack.Top
                                else stackIsEmptyFlag <- true
                            operStack.Push operations.[a]
                            i <- i + 1
                else 
                    if (Char.IsLetter(str.[i])) then
                        let mutable buf = ""
                        let mutable noSuchVarflag = true
                        while (i < str.Length) && (Char.IsLetter(str.[i])) do
                            buf <- buf + str.[i].ToString()
                            i <- i + 1
                        for j = 0 to pars.Length - 1 do
                            if (fst pars.[j] = buf) then
                                out.Push (Data (snd pars.[j]))
                                noSuchVarflag <- false
                        if (noSuchVarflag) then raise(ErrorWithParam("There's no such variable as ", buf))
                    else i <- i + 1
    while not (operStack.IsEmpty) do
        let cur = fst operStack.Top
        uniteIntoBiggerTree cur

        operStack.Pop
    if not (out.Self.Length = 1) then raise (Error ("Not enough operators"))
    out.Top
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

let calculateString (str : string) (pars : (string * int) [])= 
    try
        (calculateTree (expressionToTreeParam (str, pars))).ToString()
    with
    | Error(msg) -> msg
    | ErrorWithParam(msg, param) -> (msg + param)
//TestCase doesn't like arrays of pairs. And empty arrays. 
//So when there're no variables, I'll be passing to it "unusedVariable" with value "-1", so it will work.
[<TestCase ("(34 + 81) * 59 / 134 - (35 - 31) ^ 3", [|"unusedVariable"|], [|-1|],Result = "-14", 
    TestName = "Expression without variables 1")>]

[<TestCase ("1 + (2 / 3) ^2", [|"unusedVariable"|], [|-1|],Result = "1", 
    TestName = "Expression without variables 2")>]

[<TestCase ("(98 /3)*(-4)^2 + ((-64)%  5) ^ 3", [|"unusedVariable"|], [|-1|],Result = "448", 
    TestName = "Expression without variables 3")>]

[<TestCase ("(-19) * ((- +2) / 3) ^2", [|"unusedVariable"|], [|-1|],Result = "There can't be two operators in a row", 
    TestName = "Expression with two operators in a row")>]

[<TestCase ("1 + (2 / 3 ^2", [|"unusedVariable"|], [|-1|],Result = "Not enough integer data", //'cause '(' will be considered
                                                                                             // as operator and will be put into tree, 
    TestName = "Expression with mismatching parenthesis //no ')'")>]                        //taking '1' for itself and leaving empty stack for '+'

[<TestCase ("1 + 2 / 3) ^2", [|"unusedVariable"|], [|-1|],Result = "Mismatching parenthesis", 
    TestName = "Expression with mismatching parenthesis //no '('")>]

[<TestCase (")(98 /3)*(-4)^2 + ((-64)-  5) ^ 3", [|"unusedVariable"|], [|-1|],Result = "Mismatching parenthesis", 
    TestName = "')' at the beginning")>]

[<TestCase ("(98 /3)*(-4)^2 + ((-64)&  5) ^ 3", [|"unusedVariable"|], [|-1|],Result = "Not enough operators", 
    TestName = "Expression without variables with wrong operator")>]

[<TestCase ("(98 /3)*(-a)^2 + ((-64)%  5) ^ boy", [|"a"; "boy"|], [|4; 3|],Result = "448", 
    TestName = "Expression with variables 1")>]

[<TestCase ("(sd^(7 / 3) + (-86)) % ((2)* (s))", [|"sd"; "s"|], [|56; -17|],Result = "24", 
    TestName = "Expression with variables 2")>]

[<TestCase ("(98 /3)*(-a)^ex + ((-64)%  5) ^ boy", [|"a"; "boy"|], [|4; 3|],Result = "There's no such variable as ex", 
    TestName = "Expression without needed variable")>]

let testFunc (str : string) (vars: string []) (varsVals : int[]) =
    let pars = [|for i in 0.. vars.Length - 1 -> (vars.[i], varsVals.[i])|]
    calculateString str pars

[<EntryPoint>]
let main argv = 
  let a = calculateString "(56^(7 / 3) + (-86)) % ((2)* (-17))" [||]
  printf "%A" a
  0
