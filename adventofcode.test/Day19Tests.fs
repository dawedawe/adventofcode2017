module Day19Tests

open Xunit
open Day19
open System

[<Fact>]
let testParseSymbol () =
    let s = Day19.parseSymbol ' ' 23 42
    Assert.True(Option.isNone s.pipe)
    Assert.True(s.x = 23)
    Assert.True(s.y = 42)
    
    let s2 = Day19.parseSymbol '|' 23 42
    Assert.True(s2.pipe.Value = Vertical)

    let s3 = Day19.parseSymbol '-' 23 42
    Assert.True(s3.pipe.Value = Horizontal)
    
    let s4 = Day19.parseSymbol '+' 23 42
    Assert.True(s4.pipe.Value = Junction)

    
    let s5 = Day19.parseSymbol 'X' 23 42
    Assert.True(s5.pipe.Value = Character 'X')

[<Fact>]
let testParseLine () =
    let line = "-|+X "
    let parsedLine = Day19.parseLine line 3
    Assert.True(parsedLine.[0].x = 0)
    Assert.True(parsedLine.[0].y = 3)
    Assert.True(parsedLine.[0].pipe.Value = Horizontal)
    Assert.True(parsedLine.[1].pipe.Value = Vertical)
    Assert.True(parsedLine.[2].pipe.Value = Junction)
    Assert.True(parsedLine.[3].pipe.Value = Character 'X')
    Assert.True(parsedLine.[4].pipe.IsNone)


[<Fact>]
let testParseLines () =
    let lines = ["-|+X "; "--B"]
    let network = Day19.parseLines lines
    Assert.True(network.Count = 2)
    Assert.True(network.[0].[1].x = 1)
    Assert.True(network.[0].[1].y = 0)
    Assert.True(network.[0].[1].pipe.Value = Vertical)
    Assert.True(network.[1].[2].x = 2)
    Assert.True(network.[1].[2].pipe.Value = Character 'B')
    Assert.True(network.[1].[0].y = 1)