module Day22

open System

[<Literal>]
let InfectedSymbol = '#'

type Turn =
    | Left
    | Right

type Direction =
    | North
    | South
    | West
    | East

type Node = { infected : bool }

let adjustDirection direction turn =
    match direction with
    | North -> if turn = Left then West else East
    | South -> if turn = Left then East else West
    | West -> if turn = Left then South else North
    | East -> if turn = Left then North else South

let getDirection node =
    if (node.infected)
    then Right
    else Left

let decideInfection node =
    { node with infected = not node.infected}

let burst currentNode =
    let turn = getDirection currentNode
    let currentNode' = decideInfection currentNode
    turn, currentNode'

let parseLine (line : string) =
    let charToNode c = {infected = (c = InfectedSymbol)}
    line.ToCharArray() |> Array.map charToNode

let isNewInfection oldState newState =
    (not oldState.infected) && (newState.infected)

let calcNewPosition currentX currentY dir =
    match dir with
    | North -> currentY - 1, currentX
    | South -> currentY + 1, currentX
    | West -> currentY, currentX - 1
    | East -> currentY, currentX + 1

let adjustGrid (grid : Node [][]) y x =
    let xLength = Array.length grid.[0]
    let yLength = Array.length grid

    if y < 0
    then let newLine = Array.init xLength (fun _ -> { infected = false } )
         Array.append [|newLine|] grid
    else if y >= Array.length grid
    then let newLine = Array.init xLength (fun _ -> { infected = false } )
         Array.append grid [|newLine|]
    else if x < 0
    then for r in [0 .. (yLength - 1)] do
            grid.[r] <- Array.append [|{infected = false}|] grid.[r]
         grid
    else if x >= xLength
    then for r in [0 .. (yLength - 1)] do
            grid.[r] <- Array.append grid.[r] [|{infected = false}|]
         grid
    else grid

let adjustPosition x =
    if x < 0
    then 0
    else x

let day22 () =
    let input = System.IO.File.ReadAllLines "Day22Input.txt"
    let mutable grid = Array.map parseLine input
    let mutable x = Array.length grid.[0] / 2
    let mutable y = Array.length grid / 2
    let startNode = grid.[y].[x]

    let mutable causedInfections = 0
    let mutable currentNode = startNode
    let mutable direction = North
    for _ in [1 .. 10000] do
        let turn, currentNode' = burst currentNode
        grid.[y].[x] <- currentNode'
        direction <- adjustDirection direction turn
        if isNewInfection currentNode currentNode'
        then causedInfections <- causedInfections + 1
        let y', x' = calcNewPosition x y direction
        grid <- adjustGrid grid y' x'
        y <- adjustPosition y'
        x <- adjustPosition x'
        currentNode <- grid.[y].[x]
    causedInfections