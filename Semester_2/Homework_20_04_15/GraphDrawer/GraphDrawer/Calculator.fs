module Calc

open System
exception Error of string
exception ErrorWithParam of string * string
type CalcTree = Nil | Oper of char * CalcTree * CalcTree | Data of float | Word of string
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

let expressionToTreeParam (str : string)  =
    let mutable i = 0
    let mutable pars : (string * float) [] = [||]
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
        | Error(msg) -> raise(Error ("Not enough float data"))

    let stringPartToFloat start = 
        let mutable start = start
        let mutable buf = ""
        while ((start < str.Length) && ((Char.IsDigit(str.[start])))) do 
            buf <- buf + str.[start].ToString()
            start <- start + 1
        if ((start < str.Length) && (str.[start] = '.')) then 
          buf <- buf + "."
          start <- start + 1
          while ((start < str.Length) && ((Char.IsDigit(str.[start])))) do 
            buf <- buf + str.[start].ToString()
            start <- start + 1
        let buf = float buf
        (buf, start)

    while (i < str.Length) do 
        match str.[i] with
        | '(' -> 
            operStack.Push ('(', -1)
            let mutable j = i + 1
            let mutable mayBeNumberFlag = true
            let mutable isScanningOverFlag = false
            let mutable resIsVarFlag = false
            let mutable res = 0.0
            let mutable var = ""
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
                                let temp = stringPartToFloat j
                                res <- fst temp
                                j <- snd temp
                                isScanningOverFlag <- true
                            else 
                                if (Char.IsLetter(str.[j])) then
                                   let mutable buf = ""
                                   while (j < str.Length) && (Char.IsLetter(str.[j])) do
                                       buf <- buf + str.[j].ToString()
                                       j <- j + 1
                                   var <- buf
                                   resIsVarFlag <- true
                                   isScanningOverFlag <- true
                                else
                                    raise (Error ("There can't be two operators in a row"))
                | _ -> mayBeNumberFlag <- false
            i <- j
            if (mayBeNumberFlag) then 
              if (resIsVarFlag) then
                out.Push (Word ("-" + var))
                pars <- Array.append pars [|(var, -1.0)|]
                  //the value is irrelevant for it'll be replaced later
              else
                out.Push (Data (- res))    
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
                let temp = stringPartToFloat i
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
                        while (i < str.Length) && (Char.IsLetter(str.[i])) do
                            buf <- buf + str.[i].ToString()
                            i <- i + 1
                        out.Push (Word (buf))
                        pars <- Array.append pars [|(buf, -1.0)|]
                    else i <- i + 1
    while not (operStack.IsEmpty) do
        let cur = fst operStack.Top
        uniteIntoBiggerTree cur

        operStack.Pop
    if not (out.Self.Length = 1) then raise (Error ("Not enough operators"))
    (out.Top, pars)


let myPown (x :float) (y : float) = 
  pown x (int y)
let rec calculateTree tree (pars: (string * float) []) = 
    match tree with
    | Data (x) -> x
    | Word (x) ->
      let mutable res = 0.0
      let mutable x = x
      let mutable isNegative = false
      if (x.[0] = '-') then 
        isNegative <- true
        x <- x.[1..x.Length - 1]
      let temp = x
      if Array.exists (fun e -> fst e = temp) pars then 
        let index = Array.findIndex (fun e -> fst e = temp) pars
        res <- snd pars.[index]
        if isNegative then res <- - res
      else
        raise (Error ("There can't be two variables in an expression"))
      res
    | Nil -> raise (Error ("The tree is empty, which is wrong")) 
                   //In the current state of code this error will never be raised, but let it be.
    | Oper (x, l, r) ->
        let oper =  match x with
                    | '*' -> ((*) : float -> float -> float)
                    | '/' -> (/)
                    | '-' -> (-)
                    | '+' -> (+)
                    | '%' -> (%)
                    | '^' -> (myPown)
                    | _ -> raise (ErrorWithParam ("Unknown operator: ", x.ToString()))
                            //In the current state of code this error will never be raised, but let it be.
        let arg1 = calculateTree l pars
        let arg2 = calculateTree r pars
        if (x = '/') && (arg2 = 0.0) then raise (Error ("Division by zero!")) 
        oper arg1 arg2

let calculateString (str : string) (pars : (string * float) [])= 
    try
        (calculateTree (fst (expressionToTreeParam str)) pars).ToString()
    with
    | Error(msg) -> msg
    | ErrorWithParam(msg, param) -> (msg + param)