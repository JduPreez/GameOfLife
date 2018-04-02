module GameOfLife.Test.DomainTests

open Xunit
open GameOfLife.Domain
open GameOfLife.Domain.Entities

let worldMap = ["##.......#";   // 0
                "..#.......";   // 1
                "###...#...";   // 2
                ".....#.#..";   // 3
                "......#..#";   // 4
                ".....###..";   // 5
                "..##......";   // 6
                ".###..##..";   // 7
                "..#.......";   // 8
                ".........#";]  // 9

[<Fact>]
let ``Services.createWorld should create a world from a map``() =
  let world = Services.createWorld worldMap

  Assert.True(world.live.Contains(Live(0, 1)))
  Assert.True(world.live.Contains(Live(2, 0)))
  Assert.True(world.live.Contains(Live(2, 1)))
  Assert.True(world.live.Contains(Live(2, 2)))

[<Fact>]
let ``Services.adjacent should count the number of adjacent cells``() =
  let world = Services.createWorld worldMap

  Assert.Equal(1, Services.adjacent world (Live(0, 0)))
  Assert.Equal(3, Services.adjacent world (Live(5, 6)))
  Assert.Equal(5, Services.adjacent world (Live(7, 2)))
  Assert.Equal(0, Services.adjacent world (Live(9, 9)))

[<Fact>]
let ``Services.growCell should grow cell if it has 2 or 3 neighbours``() =
  let world = Services.createWorld worldMap

  match Services.growCell world (Live(5, 6)) with
  | Growing(Live _) -> Assert.True(true)
  | _               -> Assert.True(false)

  match Services.growCell world (Live(6, 3)) with
  | Growing(Live _) -> Assert.True(true)
  | _               -> Assert.True(false)

  match Services.growCell world (Live(8, 2)) with
  | Growing(Live _) -> Assert.True(true)
  | _               -> Assert.True(false)

[<Fact>]
let ``Services.growCell should grow a dead cell if it has 3 neighbours``() =
  let world = Services.createWorld worldMap

  match Services.growCell world (Live(8, 1)) with
  | Growing(Live _) -> Assert.True(true)
  | _               -> Assert.True(false)

  match Services.growCell world (Live(6, 1)) with
  | Growing(Live _) -> Assert.True(true)
  | _               -> Assert.True(false)

[<Fact>]
let ``Services.growCell should let a cell die with fewer than two neighbours``() =
  let world = Services.createWorld worldMap

  match Services.growCell world (Live(0, 9)) with
  | Growing(Dead _) -> Assert.True(true)
  | _               -> Assert.True(false)

  match Services.growCell world (Live(7, 7)) with
  | Growing(Dead _) -> Assert.True(true)
  | _               -> Assert.True(false)

[<Fact>]
let ``Services.growCell should grow T2 cells``() =
  let world = Services.createWorld ["..........";   // 0
                                    "..........";   // 1
                                    "..........";   // 2
                                    "..#.#.....";   // 3
                                    "...##.....";   // 4
                                    "...#......";   // 5
                                    "..........";   // 6
                                    "..........";   // 7
                                    "..........";   // 8
                                    "..........";]  // 9

  match Services.growCell world (Dead(3, 5)) with
  | Growing(Dead _) -> Assert.True(true)
  | _               -> Assert.True(false)

  match Services.growCell world (Dead(4, 5)) with
  | Growing(Dead _) -> Assert.True(true)
  | _               -> Assert.True(false)

  match Services.growCell world (Live(3, 2)) with
  | Growing(Dead _) -> Assert.True(true)
  | _               -> Assert.True(false)

  match Services.growCell world (Dead(5, 4)) with
  | Growing(Live _) -> Assert.True(true)
  | _               -> Assert.True(false)