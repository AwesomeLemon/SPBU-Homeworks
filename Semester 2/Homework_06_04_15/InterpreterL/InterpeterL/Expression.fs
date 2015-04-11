module Expression
open System
exception Error of string
exception ErrorWithParam of string * string
type CalcTree = Oper of char * CalcTree * CalcTree | Data of int | Var of string
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

let rec calculateTree tree (vars : (string * int) []) = 
    match tree with
    | Data (x) -> x
    | Var (s) ->
      let mutable res = 0
      let varExists elem =  
        match elem with
        | (a, _) -> s = a
      if (Array.exists varExists vars) then
//        let a = Array.findIndex varExists vars
        res <- snd (vars.[Array.findIndex varExists vars])
      else raise (ErrorWithParam ("Unknown variable is being used: ", s)) 
      res
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
        oper (calculateTree l vars) (calculateTree r vars)
    
let rec filePartToTree (input : IO.StreamReader) =
  //this function works with non-reversed polish notation
  //it scans only one CalcTree-type expression
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
        //In the current state of code this error will never be raised, but let it be.
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
      (res, start)
    
  buf <- input.ReadLine ()
  if (isOperator buf.[0]) then
    if (buf.[0] = '-') && (buf.Length > 1) then
      if Char.IsDigit(buf.[1]) then
        let temp = (stringPartToInt buf 1)
        if (snd temp = buf.Length) then //string contained only digits, and not something like '12as'
          out.Push (Data(- (fst temp)))
        else
          raise (ErrorWithParam("Number is incorrect: ", buf))
      else
        raise (Error("It is not digit that stands after minus"))
    else
      out.Push (filePartToTree input)
      out.Push (filePartToTree input)
      uniteIntoBiggerTree buf.[0]
  else
    if Char.IsDigit(buf.[0]) then
      let temp = (stringPartToInt buf 0)
      if (snd temp = buf.Length) then 
          out.Push (Data(fst temp))
      else
          raise (ErrorWithParam("Number is incorrect: ", buf))
    else
      out.Push (Var(buf))
  out.Top