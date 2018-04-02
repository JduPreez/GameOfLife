module GameOfLife.Views

open System.Windows.Forms
open System.ComponentModel
open System
open System.Drawing
open Domain
open Domain.Entities
open System.Threading
open GameOfLife.Domain

type WorldGridView() as me =
  inherit DataGridView()

  [<Literal>]
  let LeftMouseBtnDown = 0x201

  [<Literal>]
  let LeftMouseBtnDoubleClick = 0x203

  [<Literal>]
  let AnyKeyDown = 0x100

  do
    me.DoubleBuffered <- true

  override me.OnRowPrePaint(e: DataGridViewRowPrePaintEventArgs) =
    e.PaintParts <- (int e.PaintParts - int DataGridViewPaintParts.Focus) |> enum
    base.OnRowPrePaint(e)

  override me.WndProc(msg: Message byref) =
    match msg.Msg with
    | LeftMouseBtnDown
    | LeftMouseBtnDoubleClick
    | AnyKeyDown  -> ()
    | _           -> base.WndProc(&msg)

type GameView() as me =
  inherit Form()  
  let playBtn                       = new Button()
  let gameTypeCbx                   = new ComboBox()
  let gameGridView                  = new WorldGridView()
  let backgroundThread: Thread ref  = ref null
  let run                           = ref false
  let world                         = ref (Services.createWorld (snd defaultWorld))
  do    
    (gameGridView :> ISupportInitialize).BeginInit()
    me.SuspendLayout()
    // 
    // playBtn
    // 
    playBtn.Location                <- new Point(441, 466)
    playBtn.Name                    <- "playBtn"
    playBtn.Size                    <- new Size(75, 23)
    playBtn.TabIndex                <- 4
    playBtn.Text                    <- "Play"
    playBtn.UseVisualStyleBackColor <- true
    playBtn.Click.AddHandler(new EventHandler(fun _ _ -> me.Play()))
    // 
    // gameTypeCbx
    //
    gameTypeCbx.FormattingEnabled   <- true    
    gameTypeCbx.Location            <- new Point(12, 14)
    gameTypeCbx.Name                <- "gameTypeCbx"
    gameTypeCbx.Size                <- new Size(504, 21)
    gameTypeCbx.TabIndex            <- 3
    gameTypeCbx.Items.AddRange(games
                                |> List.map (fun game -> box(fst game))
                                |> Array.ofList)    
    gameTypeCbx.SelectedIndexChanged.AddHandler(new EventHandler(fun _ _ -> me.GameTypeSelect()))
    // 
    // gameGridView
    // 
    gameGridView.AllowUserToAddRows           <- false
    gameGridView.AllowUserToDeleteRows        <- false
    gameGridView.AllowUserToResizeColumns     <- false
    gameGridView.AllowUserToResizeRows        <- false
    gameGridView.ColumnHeadersHeightSizeMode  <- DataGridViewColumnHeadersHeightSizeMode.AutoSize
    gameGridView.ColumnHeadersVisible         <- false
    gameGridView.Location                     <- new System.Drawing.Point(12, 45)
    gameGridView.Name                         <- "gameGridView"
    gameGridView.RowHeadersVisible            <- false
    gameGridView.Size                         <- new System.Drawing.Size(503, 403)
    gameGridView.TabIndex                     <- 5
    gameGridView.AutoSizeColumnsMode          <- DataGridViewAutoSizeColumnsMode.Fill    
    // 
    // GameView
    // 
    me.AutoScaleDimensions  <- new SizeF(6.0f, 13.0f);
    me.AutoScaleMode        <- AutoScaleMode.Font
    me.ClientSize           <- new Size(529, 502)
    me.FormBorderStyle      <- System.Windows.Forms.FormBorderStyle.FixedSingle
    me.MaximizeBox          <- false
    me.Name                 <- "GameView"
    me.StartPosition        <- FormStartPosition.CenterScreen
    me.Text                 <- "Conway\'s Game of Life"
    me.Controls.Add(gameGridView)
    me.Controls.Add(playBtn)
    me.Controls.Add(gameTypeCbx)
    (gameGridView :> ISupportInitialize).EndInit()
    me.ResumeLayout(false)
    me.Shown.AddHandler(new EventHandler(fun _ _ -> me.ViewShown()))
    
    gameGridView.SuspendLayout()

    let (rows, columns) = (!world).size
    for col = 0 to columns-1 do
      gameGridView.Columns.Add("", "") |> ignore

    gameGridView.Rows.Add(rows) |> ignore

    for row = 0 to rows-1 do
      gameGridView.Rows.[row].Height <- 403/rows

    gameGridView.ResumeLayout()

  member me.Play() =
    run := true

    if !backgroundThread = null || not (!backgroundThread).IsAlive 
    then 
      backgroundThread := new Thread(new ThreadStart(me.Draw))      
      (!backgroundThread).Start()

  member private me.RedrawCell(cell: Cell, idx: int option) =    
    let (row, col) = cell.Value
    match cell with
    | Live _    -> 
      gameGridView.Rows.[row].Cells.[col].Style.BackColor <- Color.Black

    | Dead _    -> 
      gameGridView.Rows.[row].Cells.[col].Style.BackColor <- Color.White

    | Growing _ ->       
      gameGridView.Rows.[row].Cells.[col].Style.BackColor <- if idx.Value % 2 = 0
                                                              then Color.Gray
                                                              else Color.LightGray

  member me.Draw () = 
    me.Redraw(!world)

  member me.Redraw(world: World) =
    if !run then
      let (rows, columns) = world.size     
          
      let nextWorld = { world with 
                          live = seq { for row = 0 to rows-1 do
                                        for col = 0 to columns-1 do
                                          let currentCell = Services.currentCell world (row, col)
                                          let growingCell = Services.growCell world currentCell
                                          let liveCell    = Live(row, col)
                                          let futureCell = match growingCell with
                                                           | Growing(Live _ as x) -> x
                                                           | Growing(Dead _ as x) -> x
                                                           | _ -> failwith "Unexpected cell condition."
                                          
                                          if !run then me.RedrawCell(Dead(currentCell.Value), None)                                          

                                          if futureCell <> currentCell || futureCell = liveCell 
                                          then for i in 1 .. 5 do
                                                  if !run then
                                                    me.RedrawCell(growingCell, (Some i))
                                                    Thread.Sleep(15)

                                          if !run then me.RedrawCell(futureCell, None)

                                          if futureCell = liveCell
                                          then yield futureCell } |> Set.ofSeq }     
    
      if !run then
        Thread.Sleep(TimeSpan.FromSeconds(2.0))
        me.Redraw(nextWorld)

  member me.GameTypeSelect() =
    run := false
    world :=  Services.createWorld (games
                                    |> List.find (fun game -> fst game = gameTypeCbx.SelectedItem.ToString())
                                    |> snd)

    let wrld = !world           

    let (rows, columns) = wrld.size

    for row = 0 to rows-1 do
      for col = 0 to columns-1 do               
        me.RedrawCell(Services.currentCell(wrld) (row, col), None)

  member me.ViewShown() =
    gameTypeCbx.SelectedIndex     <- 0
    gameGridView.CurrentCell      <- null
    gameGridView.ShowCellToolTips <- false