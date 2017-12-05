module Day2Tests

open System
open Xunit
open Day2

[<Fact>]
let Test1 () =
    let input = [
        [5; 1; 9; 5]
        [7; 5; 3]
        [2; 4; 6; 8]
    ]
    let output = checksum input
    Assert.True(18 = output)
