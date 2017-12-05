// r u l l d d
// r r r u u u l l l l d d d d
// r r r r r u u u u u l l l l l l d d d d d d

// former r + 2, former u +2, former l + 2, former d + 2

module Day3

    type Direction =
    | Right
    | Left
    | Up
    | Down

    type Position = { x : int; y : int }

    let goRight currentAndPos =
        let newCurrent = fst currentAndPos + 1
        let newPos = { snd currentAndPos with x = (snd currentAndPos).x + 1}
        newCurrent, newPos

    let goUp currentAndPos =
        let newCurrent = fst currentAndPos + 1
        let newPos = { snd currentAndPos with y = (snd currentAndPos).y + 1}
        newCurrent, newPos

    let goLeft currentAndPos =
        let newCurrent = fst currentAndPos + 1
        let newPos = { snd currentAndPos with x = (snd currentAndPos).x - 1}
        newCurrent, newPos

    let goDown currentAndPos =
        let newCurrent = fst currentAndPos + 1
        let newPos = { snd currentAndPos with y = (snd currentAndPos).y - 1}
        newCurrent, newPos


    let goRightPath (currentAndPos : int * Position) (limit : int) (round : int) =
        let mutable newCurrentAndNewPos = currentAndPos
        let mutable i = 1
        while i <= 1 + 2 * round && fst newCurrentAndNewPos <> limit do
            newCurrentAndNewPos <- goRight newCurrentAndNewPos
            i <- i + 1
        newCurrentAndNewPos

    let goUpPath (currentAndPos : int * Position) (limit : int) (round : int) =
        let mutable newCurrentAndNewPos = currentAndPos
        let mutable i = 1
        while i <= 1 + 2 * round && fst newCurrentAndNewPos <> limit do
            newCurrentAndNewPos <- goUp newCurrentAndNewPos
            i <- i + 1
        newCurrentAndNewPos

    let goLeftPath (currentAndPos : int * Position) (limit : int) (round : int) =
        let mutable newCurrentAndNewPos = currentAndPos
        let mutable i = 1
        while i <= 2 + 2 * round && fst newCurrentAndNewPos <> limit do
            newCurrentAndNewPos <- goLeft newCurrentAndNewPos
            i <- i + 1
        newCurrentAndNewPos
    
    let goDownPath (currentAndPos : int * Position) (limit : int) (round : int) =
        let mutable newCurrentAndNewPos = currentAndPos
        let mutable i = 1
        while i <= 2 + 2 * round && fst newCurrentAndNewPos <> limit do
            newCurrentAndNewPos <- goDown newCurrentAndNewPos
            i <- i + 1
        newCurrentAndNewPos

    let spiralTo (limit : int) =
        let mutable round = 0
        let mutable currentNandPos = 1, { x = 0; y = 0 }
        while fst currentNandPos <> limit do
            currentNandPos <- goRightPath currentNandPos limit round
            currentNandPos <- goUpPath currentNandPos limit round
            currentNandPos <- goLeftPath currentNandPos limit round
            currentNandPos <- goDownPath currentNandPos limit round
            round <- round + 1
        currentNandPos

    let distance n =
        let result = spiralTo n
        let xDist = (snd result).x |> System.Math.Abs
        let yDist = (snd result).y |> System.Math.Abs
        xDist + yDist