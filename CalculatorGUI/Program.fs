open System.Windows.Forms

let exitButton (form : Form) =
  let but = new Button()
  but.Text <- "Exit"
  but.Location <- System.Drawing.Point(0, programInput.Height)
  but.Click.Add (fun e -> Application.Exit())
  but

let programLabel =
  let lbl = new Label()
  lbl.Location <- System.Drawing.Point(100, 0)
  lbl.AutoSize <- true
  lbl

let numButton n =
  let but = new Button ()
  but.Text <- n.ToString()
  but.Width <- 30
  but.Height <- 30
  but
let mainForm = 
  let form = new Form(Visible = false)
  form.Size <- new Size (250, 200)
  form.Controls.Add (exitButton form)
 // form.Controls.Add (numButton 2)
 // form.Controls.Add (programLabel)
  //form.Controls.Add (programInput)
  form
[<EntryPoint>]
let main argv = 
  mainForm.Visible <- true
  programLabel.Text <- sprintf "dfs"
  Application.Run()
  0 
