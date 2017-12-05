module Day1Tests

open System
open Xunit
open Day1

[<Fact>]
let Test1 () =
    let input = "1122"
    let output = captcha input
    Assert.True(3 = output)


[<Fact>]
let Test2 () =
    let input = "1111"
    let output = captcha input
    Assert.True(4 = output)

    
[<Fact>]
let Test3 () =
    let input = "1234"
    let output = captcha input
    Assert.True(0 = output)

[<Fact>]
let Test4 () =
    let input = "91212129"
    let output = captcha input
    Assert.True(9 = output)
