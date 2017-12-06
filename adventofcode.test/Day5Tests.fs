module Day5Tests

open Xunit

[<Fact>]
let test1 () =
    let input = [| 0; 3; 0; 1; -3 |]
    let r = Day5.processOffsets 0 0 input
    Assert.True(5 = r)
