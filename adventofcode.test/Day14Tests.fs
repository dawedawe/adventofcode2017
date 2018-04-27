module Day14Tests

open Xunit
open Day14

[<Fact>]
let findStartingPosTest1 () =
    let matrix = Array2D.create 4 4 "."
    let startingPos = findStartPos matrix
    Assert.True (Option.isNone startingPos)
    matrix.[3,3] <- "#"
    let startingPos2 = findStartPos matrix
    Assert.True (Option.isSome startingPos2)
    Assert.True (startingPos2.Value = (3,3))

[<Fact>]
let findStartingPosTest2 () =
    let matrix = Array2D.create 4 4 "#"
    let startingPos = findStartPos matrix
    Assert.True (Option.isSome startingPos)
    Assert.True (startingPos.Value = (0, 0))
