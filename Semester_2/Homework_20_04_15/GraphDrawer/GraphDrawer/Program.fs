module Drawer
open Calc
open System.Windows.Forms
open System.Drawing
open System.Windows.Forms.DataVisualization.Charting

let input =
  let textBox = new TextBox()
  textBox.Location   <- System.Drawing.Point(0, 0)
  textBox.Height <- 20
  textBox.Width  <- 200
  textBox.Text   <- "1 / x"
  textBox

let inputXY x y =
  let textBox = new TextBox()
  textBox.Location   <- System.Drawing.Point(x, y)
  textBox.Height <- 20
  textBox.Width  <- 30
  textBox.Text   <- "10"
  textBox
let xyAr = [|(inputXY 0 30); (inputXY 80 30);|]

let errorForm text =
  let form = new Form(Visible = false, Width = 300, Height = 70)
  form.Controls.Add (
    let lbl = new Label()
    lbl.Location <- System.Drawing.Point(0, 0)
    lbl.Font <- new Font( "Arial", (float32 (15.0) ))
    lbl.AutoSize <- true
    lbl.Text <- text
    lbl
  )
  form

let labelXY =
  let lbl = new Label()
  lbl.Size     <- new Size (30, 3)
  lbl.Location <- System.Drawing.Point(35, 30)
  lbl.AutoSize <- true
  lbl.Text     <-  "<= x <="
  lbl

let exitButton =
  let but = new Button()
  but.Text     <- "Exit"
  but.Anchor   <- AnchorStyles.Right
  but.Size     <- new Size (80, 20)
  but.Location <- System.Drawing.Point(285 - but.Size.Width, but.Height + 10)
  but.Click.Add (fun e -> Application.Exit())
  but

let drawAction () = 
  try
    let chart = new Chart (Dock = DockStyle.Fill)
    let form  = new Form (Width = 500, Height = 500)
    chart.ChartAreas.Add (new ChartArea("MainArea"))
    form.Controls.Add (chart)

    let mutable series = new Series (ChartType = SeriesChartType.Line)
    chart.Series.Add(series)
    series.Color <- Color.Red

    let x1 = float xyAr.[0].Text
    let x2 = float xyAr.[1].Text 

    for i in x1 .. 0.1 .. x2 do
      let treeAndVars = expressionToTreeParam input.Text
      let mutable var = ("x", 0.0) //We need variable even if there's none in the expression
      if (snd treeAndVars).Length > 0 then
          let varAndVal = (snd treeAndVars).[0]
          var <- (fst varAndVal, i)//the value in 'VarAndVal' is irrelevant
      try
        let yres = calculateTree (fst treeAndVars) [|var|]
        series.Points.AddXY (i, yres) |> ignore
      with
      | Error(msg) ->   
        if (msg = "Division by zero!") then  
                     //It means that we have a hyperbola here. 
                    //And so that it's branches won't be connected we create new series for each of them.
                   //And for some reason f# doesn't raise Divide by zero exception for float
          series <- new Series(ChartType = SeriesChartType.Line)
          chart.Series.Add(series)
          series.Color <- Color.Red
        else reraise()

    form.Visible <- true

  with
  | :? System.FormatException ->
    let f = errorForm "Wrong Input!"
    f.Visible <- true
  | Error(msg) -> 
    let f = errorForm msg
    f.Visible <- true
  | ErrorWithParam(msg, param) -> 
    let f = errorForm (msg + param)
    f.Visible <- true

let drawButton =
  let but = new Button()
  but.Text <- "Draw"
  but.Anchor <- AnchorStyles.Right
  but.Size <- new Size (80, 20)
  but.Location <- System.Drawing.Point(285 - but.Size.Width, 0)
  but.Click.Add (fun e -> drawAction())
  but

let mainForm = 
  let form = new Form(Visible = true)
  form.Size <- new Size (300, 90)
  form.Controls.Add (exitButton)
  form.Controls.Add(input)
  form.Controls.Add (drawButton)
  for i = 0 to xyAr.Length - 1 do
    form.Controls.Add(xyAr.[i])
  form.Controls.Add (labelXY)
  form
  
[<EntryPoint>]
let main argv = 
  Application.Run()
  0 
  