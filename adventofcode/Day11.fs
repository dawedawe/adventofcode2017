module Day11

open System
open System.IO

type Position = { x : int; y : int; z : int }

type Direction =
    | N
    | NE
    | SE
    | S
    | SW
    | NW

let step dir (pos : Position) : Position =
    match dir with
    | N -> { pos with y = pos.y + 1; z = pos.z - 1 }
    | NE -> { pos with x = pos.x + 1; z = pos.z - 1 }
    | SE -> { pos with x = pos.x + 1; y = pos.y - 1 }
    | S -> { pos with y = pos.y - 1; z = pos.z + 1 }
    | SW -> { pos with x = pos.x - 1; z = pos.z + 1 }
    | NW -> { pos with x = pos.x - 1; y = pos.y + 1 }

let rec go pos dirs =
    match dirs with
    | d :: rest -> let pos' = step d pos
                   go pos' rest
    | [] -> pos

let distance (pos1 : Position) (pos2 : Position) =
    let a1 = abs (pos1.x - pos2.x)
    let a2 = abs (pos1.y - pos2.y)
    let a3 = abs (pos1.z - pos2.z)
    List.max [a1; a2; a3]
    
let readPath path =
    let l = File.ReadAllText path
    l

let parseLine (line : string) =
    let a = line.Split ','
    let mutable path = [||]
    for d in a do
        let dir = match d with
                  | "n" -> N
                  | "ne" -> NE
                  | "se" -> SE
                  | "s" -> S
                  | "sw" -> SW
                  | "nw" -> NW
        path <- Array.append path [|dir|]
    List.ofArray path

let distanceAfterPath start path =
    go start path
    |> distance start

let day11 () =
    let start = { x = 0; y = 0; z = 0 }
    readPath "Day11Input.txt"
    |> parseLine
    |> distanceAfterPath start