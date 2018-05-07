module Day16

open System
open System.Collections.Generic
open System.Linq

type Move =
    | S of int
    | X of (int * int)
    | P of (char * char)

let spin dancers n =
    let startPos = Array.length dancers - n
    let dancersToMove = dancers.[startPos ..]
    Array.append dancersToMove dancers.[0 .. (startPos - 1)]

let exchange (dancers : 'T array) p1 p2 =
    let t = dancers.[p1]
    dancers.[p1] <- dancers.[p2]
    dancers.[p2] <- t
    dancers

let partner (dancers : char array) name1 name2 =
    let name1Pos = Array.findIndex (fun x -> x = name1) dancers
    let name2Pos = Array.findIndex (fun x -> x = name2) dancers
    exchange dancers name1Pos name2Pos

let makeMove (dancers : char array) move =
    match move with
    | S n -> spin dancers n
    | X (pos1, pos2) -> exchange dancers pos1 pos2
    | P (name1, name2) -> partner dancers name1 name2

let dance moves =
    let mutable dancers = [for x in [97 .. 112] do yield char(x)] |> List.toArray
    for m in moves do
        dancers <- makeMove dancers m
    Array.fold (fun x y -> string(x) + string(y)) "" dancers

let danceLoop moves n =
    let mutable dancers = [for x in [97 .. 112] do yield char(x)] |> List.toArray
    let mutable i = 1
    while i <= n do
        printfn "%d" i
        i <- i + 1
        for m in moves do
            dancers <- makeMove dancers m
    Array.fold (fun x y -> string(x) + string(y)) "" dancers

let danceTillAtStartAgain moves =
    let mutable dancers = [for x in [97 .. 112] do yield char(x)] |> List.toArray
    let positions = Dictionary<int, char []>()
    let mutable i = 1
    positions.Add(i, Array.copy dancers)
    for m in moves do
            dancers <- makeMove dancers m
    while (not (positions.Any(fun kv -> kv.Value = dancers))) do
        i <- i + 1
        positions.Add(i, Array.copy dancers)
        for m in moves do
            dancers <- makeMove dancers m
    positions

let parseMove (moveString : string) =
    let moveType = moveString.[0]
    let args = moveString.[1..].Split [|'/'|]
    match moveType with
    | 's' -> S (int args.[0])
    | 'p' -> P (args.[0].[0], args.[1].[0])
    | 'x' -> X ((int args.[0]), (int args.[1]))
    | _ -> raise(Exception("unknown move"))

let parseMoves (input : string) =
    let moveStrings = input.Split [|','|]
    Array.map parseMove moveStrings

let day16 () =
    let input = System.IO.File.ReadAllText "Day16Input.txt"
    let moves = parseMoves input
    let dancers = dance moves
    dancers

let day16Part2 () =
    let input = System.IO.File.ReadAllText "Day16Input.txt"
    let moves = parseMoves input
    let positions = danceTillAtStartAgain moves
    let i = 1000000000 % positions.Count
    let dancers = positions.[i+1]
    Array.fold (fun x y -> string(x) + string(y)) "" dancers
    