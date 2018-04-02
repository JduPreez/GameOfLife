module GameOfLife.Domain

module Entities =

  type Column = int
  type Row = int

  type Cell =
  | Live    of Row * Column
  | Dead    of Row * Column
  | Growing of Cell
  with
    member me.Value =
      match me with
      | Live(r, c)          -> r, c
      | Dead(r, c)          -> r, c
      | Growing(Live(r, c)) -> r, c
      | Growing(Dead(r, c)) -> r, c
      | _                   -> failwith "Unexpected cell condition."

  type WorldSize =
  | Height
  
  type World = {
    live:     Set<Cell>
    size:     Row * Column
  }

  let games =
    ["Chaos",         [ ".#.........#.........#........";
                        "..#.........#.........#.......";
                        "###...#...###...#...###...#...";
                        ".....#.#.......#.#.......#.#..";
                        "......#.........#.........#...";
                        ".....###...###..##...###..##..";
                        "..##...........###.......###..";
                        ".###..##.......#.#......#.#...";
                        "...........###..##.......#.#.#";
                        "...............#.#......#.#.##";];
    "R-pentomnino",  [  "..............................";
                        "..............................";
                        "..............................";
                        "..............................";
                        "..............##..............";
                        ".............##...............";
                        "..............#...............";
                        "..............................";
                        "..............................";
                        ".............................."; ];
    "Pulsarish",      [ ".........###...###............";
                        "..............................";
                        ".......#....#.#....#..........";
                        ".......#....#.#....#..........";
                        ".......#....#.#....#..........";
                        ".........###...###............";
                        "..............................";
                        ".........###...###............";
                        ".......#....#.#....#..........";
                        ".......#....#.#....#.........."; ];
    "Glider",         [ "..............................";
                        "..............................";
                        "..............................";
                        "............#.................";
                        ".............#................";
                        "...........###................";
                        "..............................";
                        "..............................";
                        "..............................";
                        ".............................."; ]]

  let defaultWorld = List.head games   

module Services = 
  open Entities

  let private indexMap (worldMap: string list) =
    worldMap
    |> List.mapi (fun rowIdx row ->
        row
        |> Seq.toList
        |> List.mapi (fun colIdx cell -> (cell, rowIdx, colIdx)))
    |> List.concat

  let createWorld (worldMap: string list): World =
    let indexedMap = indexMap worldMap

    { size    = (List.length worldMap, String.length worldMap.[0])
      live    = indexedMap
                |> List.filter (fun (cellVal, _, _) -> cellVal = '#' || cellVal = '+')
                |> List.map (fun (_, i, j) -> Live(i, j))
                |> Set.ofList }

  let adjacent (world: World) (cell: Cell) =
    let (row, col) = cell.Value
    [ for r in row-1 .. row+1 do
        for c in col-1 .. col+1 do
          if Live(r, c) <> cell then yield (r, c) ]
    |> List.sumBy (fun (r, c) ->
                    match Set.contains (Live (r, c)) world.live with 
                    | true -> 1
                    | _ -> 0)
  
  let growCell (world: World) (cell: Cell) =
    match Set.contains cell world.live, adjacent world cell with
    | (false, adjcnt) when adjcnt = 3 ->                Growing(Live cell.Value)
    | (true, adjcnt)  when adjcnt = 2 || adjcnt = 3 ->  Growing(Live cell.Value)
    | _ ->                                              Growing(Dead cell.Value)

  let currentCell (world: World) ((row, col): Row * Column) =    
    let liveCell = Live(row, col)

    if Set.contains liveCell world.live
    then liveCell
    else Dead(row, col)

