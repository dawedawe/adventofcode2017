module Day21Tests

open Xunit
open Day21
open System

[<Fact>]
let testIsMatchingRule () =
    let rule = parseRuleLine ".#./..#/### => #..#/..../..../#..#"
    let startPattern : Pattern = array2D [|
                                            [| '.'; '#'; '.' |];
                                            [| '.'; '.'; '#' |];
                                            [| '#'; '#'; '#' |];
                                         |]
    let r = isMatchingRule rule startPattern
    Assert.True(r)

[<Fact>]
let testIsMatchingRule2 () =
    let rules = Array.map parseRuleLine [|".#./..#/### => #..#/..../..../#..#"|]
    let startPattern : Pattern = array2D [|
                                            [| '.'; '#'; '.' |];
                                            [| '.'; '.'; '#' |];
                                            [| '#'; '#'; '#' |];
                                         |]
    let transitions = [id; rotateCw90; rotateCcw90; rotate180; flipH; flipV; flipD]
    for t in transitions do
        let pattern = t startPattern
        let matchingRule = findMatchingRule rules transitions pattern
        let o = snd matchingRule
        Assert.True((Array2D.length1 o = 4))