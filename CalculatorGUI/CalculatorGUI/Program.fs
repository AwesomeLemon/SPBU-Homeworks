module simpleCalc
open System.Windows.Forms
open System.Drawing

let mainWidth = 300
let mainHeight = 300

let programLabel =
  let lbl = new Label()
  lbl.Size <- new Size (100, 3)
  lbl.Location <- System.Drawing.Point(0, 0)
  lbl.Font <- new Font( "Arial", (float32 (17.0) ))
  lbl.AutoSize <- true
  lbl
let myTryParse (s : string) =
  let mutable res = true
  if not ((System.Char.IsDigit s.[0]) || (s.[0] = '-' )) then res <- false
  for i = 1 to s.Length - 1 do
    if not (System.Char.IsDigit s.[i]) then res <- false
  res
type Calc () =  
  class
    let mutable a1 = ""
    let mutable a2 = ""
    let mutable op = ""
    let mutable isReady = false
    let mutable arg1Scanned = false
    let mutable arg2Scanned = false

    member this.arg1 () = a1
    member this.arg2 () = a2
    member this.oper () = op

    member this.Arg1IsScanned () = arg1Scanned
    member this.Arg1Scanned () = arg1Scanned <- true

    member this.Arg2IsScanned () = arg2Scanned
    member this.Arg2Scanned () = arg2Scanned <- true

    member this.IsReady () = isReady
    member this.Ready () = isReady <- true

    member this.changeA (x : string) v =
      match x with
      | "a1" -> a1 <- v
      | "a2" -> a2 <- v
      | _ -> () //In the current state of the code this line will never be executed
    member this.changeOp v = 
      op <- v
    member this.Clean () =
       a1 <- ""
       a2 <- ""
       op <- ""
       isReady <- false
       arg1Scanned <- false
       arg2Scanned <- false
    member this.Calculate () =
      let mutable res = ""
      let o = 
        match op with
        | "+" -> (+)
        | "-" -> (-)
        | "*" -> (*)
        | "/" -> (/)
        | _ -> (+) //In the current state of the code this line will never be executed
      let mutable ar1 = 0
      let mutable ar2 = 0
      if not (myTryParse a1) then 
        res <- "Incorrect input"
        this.Clean ()
      else
        ar1 <- int a1
        if not (myTryParse a2) then 
          res <- "Incorrect input"
          this.Clean ()
        else
          ar2 <- int a2
          if (op = "/") && (ar2 = 0) then 
              res <- "Don't divide by zero"
              a1 <- ""
              arg1Scanned <- false
          else 
            a1 <- (o ar1 ar2).ToString()
            res <- a1
      //next section of code is just like this.Clean, except that value in 'a1' isn't erased
      a2 <- ""
      op <- ""
      isReady <- false
      arg1Scanned <- false
      arg2Scanned <- false

      res
  end
let cl = new Calc()

let scanNum (x : char) = 
  if not (cl.Arg1IsScanned ()) then
    cl.changeA "a1" (cl.arg1() + x.ToString())
  else
    if not (cl.Arg2IsScanned ()) then
      cl.changeA "a2" (cl.arg2() + x.ToString())

let scanOp (c : char) = 
  match c with
  | '=' -> 
    cl.Ready ()
    cl.Arg2Scanned ()
  | _ -> 
    if (cl.oper()).Length <> 0 then 
      programLabel.Text <- "Two operators in a row"
    else
      cl.changeOp (c.ToString())
      cl.Arg1Scanned ()

let exitButton (form : Form) =
  let but = new Button()
  but.Text <- "Exit"
  but.Size <- new Size (80, 20)
  but.Location <- System.Drawing.Point(mainWidth - but.Size.Width - 15, 0)
  but.Click.Add (fun e -> Application.Exit())
  but
let cleanAction (but : Button) = 
  cl.Clean()
  programLabel.Text <- ""
let cleanButton () =
  let but = new Button()
  but.Text <- "Clean all"
  but.Size <- new Size (80, 40)
  but.Location <- System.Drawing.Point(mainWidth - but.Size.Width - 15, 30)
  but.Click.Add (fun e -> cleanAction but)
  but

let nooAction (but : Button) =
  if (System.Char.IsDigit but.Text.[0]) then
    scanNum but.Text.[0]
  else
    if not (cl.Arg1IsScanned ()) && (but.Text.[0] = '-') then 
      if (cl.arg1()).Length = 0 then 
        cl.changeA "a1" (cl.arg1 () + "-")
      else scanOp but.Text.[0]
    else
      if (cl.oper () = "") || (but.Text.[0] = '=') then scanOp but.Text.[0]
      //if not (cl.Arg2IsScanned ()) && (but.Text.[0] = '-') then cl.changeA "a2" (cl.arg2 () + "-")
      else
      cl.changeA "a2" (cl.arg2 () + "-")
  if (cl.IsReady()) then
    programLabel.Text <- cl.Calculate ()

let nooButton n x y = //Number or Operator Button
  let but = new Button ()
  but.Text <- n.ToString()
  but.Width <- 30
  but.Height <- 30
  but.Location <- System.Drawing.Point(x, y) 
  but.Click.Add ( fun a ->
    if (programLabel.Text.Length >= 2) then
      if (System.Char.IsLetter programLabel.Text.[1]) then programLabel.Text <- ""
    programLabel.Text <- programLabel.Text + but.Text
    nooAction but)
  but
let mainForm = 
  let form = new Form(Visible = false)
  form.Size <- new Size (mainWidth, mainHeight)
  form.Controls.Add (exitButton form)
  let mutable curx = 0
  let trueX = curx
  let mutable cury = mainHeight - 150
  let trueY = cury
  for i = 1 to 3 do
    for j = 1 to 3 do
      form.Controls.Add (nooButton (j + (i - 1) * 3) curx cury)
      curx <- curx + 40
    curx <- trueX
    cury <- cury - 40
  curx <- trueX + 120
  cury <- trueY + 40
  form.Controls.Add (nooButton '+' curx cury)
  cury <- cury - 40
  form.Controls.Add (nooButton '-' curx cury)
  cury <- cury - 40
  form.Controls.Add (nooButton '*' curx cury)
  cury <- cury - 40
  form.Controls.Add (nooButton '/' curx cury)
  curx <- curx + 40
  form.Controls.Add (nooButton '=' curx cury)
  form.Controls.Add (nooButton 0 (trueX + 40) (trueY + 40))
  form.Controls.Add (programLabel)
  form.Controls.Add (cleanButton () )
  //form.Controls.Add (programInput)
  form


let test1 (s1 : string) (s2 : string) (op : char) =
  for i = 0 to s1.Length - 1 do
    nooAction (nooButton (s1.[i].ToString()) 0 0)
  nooAction (nooButton op 0 0)
  for i = 0 to s2.Length - 1 do
    nooAction (nooButton (s2.[i].ToString()) 0 0)
  nooAction (nooButton '=' 0 0)
  programLabel.Text

[<EntryPoint>]
let main argv = 
  mainForm.Visible <- true
  Application.Run()
  //printf "%A" (test1 "-2" "-3" '*')
  0 
  