module Day9

open System.Collections.Generic
open System.Linq

type Structure =
| GroupStart
| GroupEnd
| GarbageStart
| GarbageEnd

let rec isInsideGarbage (sofar : Structure array) =
    sofar.Length > 0 && Array.last sofar = GarbageStart

let rec parserHelper (sofar : Structure array) (input : IEnumerable<char>) =
    if input.Count() = 0
    then sofar
    else
        match input.ElementAt(0) with
        | '{' -> if isInsideGarbage sofar
                 then parserHelper sofar (input.Skip 1)
                 else let sofar' = Array.append sofar [| GroupStart |]
                      parserHelper sofar' (input.Skip 1)
        | '}' -> if isInsideGarbage sofar
                 then parserHelper sofar (input.Skip 1)
                 else let sofar' = Array.append sofar [| GroupEnd |]
                      parserHelper sofar' (input.Skip 1)
        | '<' -> if isInsideGarbage sofar
                 then parserHelper sofar (input.Skip 1)
                 else let sofar' = Array.append sofar [| GarbageStart |]
                      parserHelper sofar' (input.Skip 1)
        | '>' -> let sofar' = Array.append sofar [| GarbageEnd |]
                 parserHelper sofar' (input.Skip 1)
        | '!' -> parserHelper sofar (input.Skip 2)
        | _ -> parserHelper sofar (input.Skip 1)

let getLine path =
    System.IO.File.ReadAllLines path
    |> Array.item 0

let groupScore (groups : Structure array) =
    let mutable level = 0
    let mutable sum = 0
    for g in groups do
        match g with
        | GroupStart -> level <- level + 1
                        sum <- sum + level
                        ()
        | GroupEnd -> level <- level - 1
                      ()
        | _ -> ()
    sum

let parse (input : string) =
    parserHelper [||] (input.ToCharArray())

let parseAndScore =
    parse >> groupScore

let day9 () =
    getLine "Day9Input.txt"
    |> parseAndScore

let rec parseGarbageAmount (sofar : Structure array) (count : int) (input : IEnumerable<char>) =
    if input.Count() = 0
    then count
    else
        match input.ElementAt(0) with
        | '{' -> if isInsideGarbage sofar
                 then parseGarbageAmount sofar (count + 1) (input.Skip 1) 
                 else let sofar' = Array.append sofar [| GroupStart |]
                      parseGarbageAmount sofar' count (input.Skip 1)
        | '}' -> if isInsideGarbage sofar
                 then parseGarbageAmount sofar (count + 1) (input.Skip 1)
                 else let sofar' = Array.append sofar [| GroupEnd |]
                      parseGarbageAmount sofar' count (input.Skip 1)
        | '<' -> if isInsideGarbage sofar
                 then parseGarbageAmount sofar (count + 1) (input.Skip 1)
                 else let sofar' = Array.append sofar [| GarbageStart |]
                      parseGarbageAmount sofar' count (input.Skip 1)
        | '>' -> let sofar' = Array.append sofar [| GarbageEnd |]
                 parseGarbageAmount sofar' count (input.Skip 1)
        | '!' -> parseGarbageAmount sofar count (input.Skip 2)
        | _ -> if isInsideGarbage sofar
               then parseGarbageAmount sofar (count + 1) (input.Skip 1)
               else parseGarbageAmount sofar count (input.Skip 1)

let day9Part2 () =
    getLine "Day9Input.txt"
    |> fun x -> x.ToCharArray()
    |> parseGarbageAmount [||] 0