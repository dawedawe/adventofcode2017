module Day19

open System
open System.Collections.Generic
open System.Linq
open System.Collections.Generic

type PipePart =
    | Vertical
    | Horizontal
    | Junction
    | Character of char

type MapPoint = {
    x : int
    y : int
    pipe : PipePart option
}

type Direction =
    | Left
    | Right
    | Up
    | Down

type MapLine = System.Collections.Generic.List<MapPoint>
type PipeMap = System.Collections.Generic.List<MapLine>

let parseSymbol (symbol : char) xIndex yIndex =
    match symbol with
    | ' ' -> { x = xIndex; y = yIndex; pipe = None}
    | '|' -> { x = xIndex; y = yIndex; pipe = Some Vertical}
    | '-' -> { x = xIndex; y = yIndex; pipe = Some Horizontal}
    | '+' -> { x = xIndex; y = yIndex; pipe = Some Junction}
    | c when Char.IsLetter c -> { x = xIndex; y = yIndex; pipe = Some (Character c) }
    | _ -> raise(Exception("unknown symbol"))

let parseLine (line : string) yIndex : MapLine =
    let parts = MapLine ()
    let mutable xIndex = 0
    for c in line do
        let part = parseSymbol c xIndex yIndex
        parts.Add(part)
        xIndex <- xIndex + 1
    parts

let parseLines lines : PipeMap =
    let mutable yIndex = 0
    let network = PipeMap ()
    for line in lines do
        parseLine line yIndex |> network.Add
        yIndex <- yIndex + 1
    network

let getStartPoint (pipeMap : PipeMap) =
    let topLine = pipeMap.[0]
    topLine.Find(fun l -> l.pipe.IsSome && l.pipe.Value = Vertical)

let isValidPointOnMap x y (pipeMap : PipeMap) =
    y >= 0 && y < pipeMap.Count && x >= 0 && x < pipeMap.[0].Count && pipeMap.[y].[x].pipe.IsSome

let isPossibleDir (pipeMap : PipeMap) currentPoint dirToTry =
    let x, y = match dirToTry with
               | Up -> currentPoint.x, currentPoint.y - 1
               | Down -> currentPoint.x, currentPoint.y + 1
               | Left -> currentPoint.x - 1, currentPoint.y
               | Right -> currentPoint.x + 1, currentPoint.y
    isValidPointOnMap x y pipeMap && pipeMap.[y].[x].pipe.IsSome

let filterWayBack currentDir =
    match currentDir with
    | Down -> [Down; Left; Right]
    | Up -> [Up; Left; Right]
    | Left -> [Up; Down; Left]
    | Right -> [Up; Down; Right]

let determineNewDir pipeMap currentPoint currentDir =
    let possibleDirs = filterWayBack currentDir
    printfn "%A" possibleDirs
    let newDir = List.filter (fun p -> isPossibleDir pipeMap currentPoint p) possibleDirs
    if newDir.Length <> 1
    then raise(Exception("no possible new direction found"))
    else newDir.[0]

let rec moveOneStep (letters : List<char>) (pipeMap : PipeMap) (currentPoint : MapPoint) (dir : Direction) =
    printfn "current %d %d %A %A" currentPoint.x currentPoint.y currentPoint.pipe.Value dir
    let newState = match (currentPoint, dir) with
                    | p, Down when (p.pipe.IsSome && (p.pipe = Some Vertical || p.pipe = Some Horizontal)) ->  (p.x, p.y + 1), Down, letters
                    | p, Up when (p.pipe.IsSome && (p.pipe = Some Vertical || p.pipe = Some Horizontal)) ->  (p.x, p.y - 1), Up, letters
                    | p, Left when (p.pipe.IsSome && (p.pipe = Some Vertical || p.pipe = Some Horizontal)) ->  (p.x - 1 , p.y), Left, letters
                    | p, Right when (p.pipe.IsSome && (p.pipe = Some Vertical || p.pipe = Some Horizontal)) ->  (p.x + 1, p.y), Right, letters

                    | p, _ when (p.pipe.IsSome) ->
                        match p.pipe.Value with
                        | Character x -> letters.Add x
                                         match dir with
                                         | Down -> (p.x, p.y + 1), dir, letters
                                         | Up -> (p.x, p.y - 1), dir, letters
                                         | Left -> (p.x - 1, p.y), dir, letters
                                         | Right -> (p.x + 1, p.y), dir, letters
                        | Junction -> let newDir = determineNewDir pipeMap currentPoint dir
                                      match newDir with
                                      | Down -> (p.x, p.y + 1), Down, letters
                                      | Up -> (p.x, p.y - 1), Up, letters
                                      | Left -> (p.x - 1, p.y), Left, letters
                                      | Right -> (p.x + 1, p.y), Right, letters
                        | _ -> raise(Exception("bad state 1"))
                    | _ -> raise(Exception("bad state 2"))
    newState |> fun ((x, y), newDir, newLetters) -> printfn "new %d %d %A" x y newDir
                                                    if (isValidPointOnMap x y pipeMap)
                                                      then moveOneStep newLetters pipeMap pipeMap.[y].[x] newDir
                                                      else newLetters

let findWay (pipeMap : PipeMap) =
    let start = getStartPoint pipeMap
    let letters = System.Collections.Generic.List<char>()
    let chars = moveOneStep letters pipeMap start Down
    let mutable s = ""
    for c in chars do
        s <- s + c.ToString()
    s
    
let day19 () =
    let input = System.IO.File.ReadAllLines("Day19Input.txt")
    let pipeMap = parseLines input
    findWay pipeMap