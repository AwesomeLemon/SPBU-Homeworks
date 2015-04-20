module Parser
open Expression
open System
type tree = 
       | Nil
       | Read   of string
       | Write  of CalcTree
       | Assign of string * CalcTree
       | Seq    of tree * tree
       | If     of CalcTree * tree * tree
       | While  of CalcTree * tree

let toTree (fin : string) = 
  use input = new IO.StreamReader (fin)

  let rec scan () = 
    let mutable buf = ""
    let mutable res = Nil 
    buf <- input.ReadLine()
    if (buf <> null) then
      res <- match buf with
              | ";" -> 
                Seq (scan (), scan ())
              | "read" -> 
                buf <- input.ReadLine()
                Read (buf)
              | ":=" -> 
                buf <- input.ReadLine()
                Assign(buf, filePartToTree input)
              | "write" ->  
                Write(filePartToTree input)
              | "while" -> 
                While (filePartToTree input, scan())
              | "if" -> 
                If (filePartToTree input, scan(), scan())
              | _ -> 
                raise (ErrorWithParam("Unknown command: ", buf))
    res

  scan ()
