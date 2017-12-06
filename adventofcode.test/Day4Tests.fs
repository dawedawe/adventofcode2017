module Day4Tests

open Xunit

[<Fact>]
let Test1 () =
    let input = "aa bb cc dd ee"
    let r = Day4.isValidPhrase input
    Assert.True(r)

[<Fact>]
let Test2 () =
    let input = "aa bb cc dd aa"
    let r = Day4.isValidPhrase input
    Assert.False(r)


[<Fact>]
let Test3 () =
    let input = "aa bb cc dd aaa"
    let r = Day4.isValidPhrase input
    Assert.True(r)