open System
open System.Windows.Forms
open GameOfLife
open GameOfLife.Views
 
[<EntryPoint>]
[<STAThread>]
let main argv = 
 
    Application.EnableVisualStyles()
    Application.SetCompatibleTextRenderingDefault false
    Application.Run(new GameView())

    0