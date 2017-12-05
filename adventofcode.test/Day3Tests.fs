module Day3Tests

open System
open Xunit
open Day3

[<Fact>]
let Test1 () =
    let output = distance 1
    Assert.True(0 = output)

[<Fact>]
let Test2 () =
    let output = distance 12
    Assert.True(3 = output)


[<Fact>]
let Test3 () =
    let output = distance 23
    Assert.True(2 = output)

    
[<Fact>]
let Test4 () =
    let output = distance 1024
    Assert.True(31 = output)