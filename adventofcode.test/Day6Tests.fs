module Day6Tests

open Xunit

[<Fact>]
let test1 () =
    let input = [| 0; 2; 7; 0 |]
    let r = Day6.distributeMemory input
    Assert.True(5 = r)

[<Fact>]
let test2 () =
    let input = [| 0; 2; 7; 0 |]
    let r = Day6.distributeMemoryBlock input 2
    Assert.True([|2; 4; 1; 2|] = r)

[<Fact>]
let test3 () =
    let input = [|2; 4; 1; 2|]
    let r = Day6.distributeMemoryBlock input 1
    Assert.True([|3; 1; 2; 3|] = r)
    
[<Fact>]
let test4 () =
    let input = [|3; 1; 2; 3|]
    let r = Day6.distributeMemoryBlock input 0
    Assert.True([|0; 2; 3; 4|] = r)

[<Fact>]
let test5 () =
    let input = [|0; 2; 3; 4|]
    let r = Day6.distributeMemoryBlock input 3
    Assert.True([|1; 3; 4; 1|] = r)


[<Fact>]
let test6 () =
    let input = [|1; 3; 4; 1|]
    let r = Day6.distributeMemoryBlock input 2
    Assert.True([|2; 4; 1; 2|] = r)
