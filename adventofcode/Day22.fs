module Day22

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

type State =
    | Clean
    | Weakened
    | Infected
    | Flagged

type Node = { state : State }

let adjustDirection direction turn =
    match direction with
    | North -> if turn = Left then West else East
    | South -> if turn = Left then East else West
    | West -> if turn = Left then South else North
    | East -> if turn = Left then North else South

let getDirection node =
    if node.state = State.Infected
    then Right
    else Left

let reverseDirection direction =
    match direction with
    | North -> South
    | South -> North
    | West -> East
    | East -> West

let transformDirectionLeft direction =
    match direction with
    | North -> West
    | South -> East
    | West -> South
    | East -> North

let transformDirectionRight direction =
    match direction with
    | North -> East
    | South -> West
    | West -> North
    | East -> South

let getDirectionPart2 node currentDirection =
    match node.state with
    | State.Clean -> transformDirectionLeft currentDirection
    | State.Flagged -> reverseDirection currentDirection
    | State.Infected -> transformDirectionRight currentDirection
    | State.Weakened -> currentDirection

let transformState node =
    let newState = if node.state = State.Infected
                   then State.Clean
                   else State.Infected
    { node with state = newState }

let transformStatePart2 node =
    match node.state with
    | State.Infected -> { node with state = Flagged }
    | State.Clean -> { node with state = Weakened }
    | State.Flagged -> { node with state = Clean }
    | State.Weakened -> { node with state = Infected }

let burst currentNode =
    let turn = getDirection currentNode
    let currentNode' = transformState currentNode
    turn, currentNode'

let burstPart2 currentNode currentDirection =
    let newDirection = getDirectionPart2 currentNode currentDirection
    let currentNode' = transformStatePart2 currentNode
    newDirection, currentNode'

let parseLine (line : string) =
    let charToNode c =
        let state = if c = InfectedSymbol
                    then State.Infected
                    else State.Clean
        { state = state }
    line.ToCharArray() |> Array.map charToNode

let isNewInfection oldState newState =
    oldState.state <> State.Infected && newState.state = State.Infected

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
    then let newLine = Array.init xLength (fun _ -> { state = State.Clean } )
         Array.append [|newLine|] grid
    else if y >= Array.length grid
    then let newLine = Array.init xLength (fun _ -> { state = State.Clean } )
         Array.append grid [|newLine|]
    else if x < 0
    then for r in [0 .. (yLength - 1)] do
            grid.[r] <- Array.append [|{ state = State.Clean }|] grid.[r]
         grid
    else if x >= xLength
    then for r in [0 .. (yLength - 1)] do
            grid.[r] <- Array.append grid.[r] [|{ state = State.Clean }|]
         grid
    else grid

let adjustPosition x =
    if x < 0
    then 0
    else x

[<Literal>]
let InputFile = "Day22Input.txt"

let constructInitialGrid filePath =
    System.IO.File.ReadAllLines filePath
    |> Array.map parseLine

let day22 () =
    let mutable grid = constructInitialGrid InputFile
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

let day22Part2 () =
    let mutable grid = constructInitialGrid InputFile
    let mutable x = Array.length grid.[0] / 2
    let mutable y = Array.length grid / 2
    let startNode = grid.[y].[x]

    let mutable causedInfections = 0
    let mutable currentNode = startNode
    let mutable direction = North
    for _ in [1 .. 10000000] do
        let direction', currentNode' = burstPart2 currentNode direction
        grid.[y].[x] <- currentNode'
        direction <- direction'
        if isNewInfection currentNode currentNode'
        then causedInfections <- causedInfections + 1
        let y', x' = calcNewPosition x y direction
        grid <- adjustGrid grid y' x'
        y <- adjustPosition y'
        x <- adjustPosition x'
        currentNode <- grid.[y].[x]
    causedInfections