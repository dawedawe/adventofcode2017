// r u l l d d
// r r r u u u l l l l d d d d
// r r r r r u u u u u l l l l l l d d d d d d

// former r + 2, former u +2, former l + 2, former d + 2

module Day3

    open System.Collections.Generic

    type Direction =
    | Right
    | Left
    | Up
    | Down

    type Position = { x : int; y : int }

    let movePositionUp pos = fst pos, snd pos + 1
    let movePositionDown pos = fst pos, snd pos - 1
    let movePositionLeft pos = fst pos - 1, snd pos
    let movePositionRight pos = fst pos + 1, snd pos

    let goRight currentAndPos =
        let newCurrent = fst currentAndPos + 1
        let newPos = { snd currentAndPos with x = (snd currentAndPos).x + 1}
        newCurrent, newPos

    let sumOfNeighbours (pos : int*int) (grid : Dictionary<int*int, int>) =
        let helper (g : Dictionary<int*int, int>) (p : int*int) = 
            if (g.ContainsKey(p))
            then g.[p]
            else 0

        let north = helper grid (movePositionUp pos)
        let northeast = helper grid (fst pos + 1, snd pos + 1)
        let east = helper grid (movePositionRight pos)
        let southeast = helper grid (fst pos + 1, snd pos - 1)
        let south = helper grid (movePositionDown pos)
        let southwest = helper grid (fst pos - 1, snd pos - 1)
        let west = helper grid (movePositionLeft pos)
        let northwest = helper grid (fst pos - 1, snd pos + 1)
        north + northeast + east + southeast + south + southwest + west + northwest

    let go goFunc (pos : int*int) (grid : Dictionary<int*int, int>) =
        let newPos = goFunc pos
        let newCurrent = sumOfNeighbours newPos grid
        grid.[newPos] <- newCurrent
        newPos

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

    let goRightPath2 (currentPos : int * int) (grid : Dictionary<int*int, int>) limit round =
        let mutable newCurrentPos = currentPos
        let mutable i = 1
        while i <= 1 + 2 * round && grid.[newCurrentPos] < limit do
            newCurrentPos <- go movePositionRight newCurrentPos grid
            i <- i + 1
        newCurrentPos

    let goUpPath (currentAndPos : int * Position) (limit : int) (round : int) =
        let mutable newCurrentAndNewPos = currentAndPos
        let mutable i = 1
        while i <= 1 + 2 * round && fst newCurrentAndNewPos <> limit do
            newCurrentAndNewPos <- goUp newCurrentAndNewPos
            i <- i + 1
        newCurrentAndNewPos

    let goUpPath2 (currentPos : int * int) (grid : Dictionary<int*int, int>) limit round =
        let mutable newCurrentPos = currentPos
        let mutable i = 1
        while i <= 1 + 2 * round && grid.[newCurrentPos] < limit do
            newCurrentPos <- go movePositionUp newCurrentPos grid
            i <- i + 1
        newCurrentPos

    let goLeftPath (currentAndPos : int * Position) (limit : int) (round : int) =
        let mutable newCurrentAndNewPos = currentAndPos
        let mutable i = 1
        while i <= 2 + 2 * round && fst newCurrentAndNewPos <> limit do
            newCurrentAndNewPos <- goLeft newCurrentAndNewPos
            i <- i + 1
        newCurrentAndNewPos
    
    let goLeftPath2 (currentPos : int * int) (grid : Dictionary<int*int, int>) limit round =
        let mutable newCurrentPos = currentPos
        let mutable i = 1
        while i <= 2 + 2 * round && grid.[newCurrentPos] < limit do
            newCurrentPos <- go movePositionLeft newCurrentPos grid
            i <- i + 1
        newCurrentPos

    let goDownPath (currentAndPos : int * Position) (limit : int) (round : int) =
        let mutable newCurrentAndNewPos = currentAndPos
        let mutable i = 1
        while i <= 2 + 2 * round && fst newCurrentAndNewPos <> limit do
            newCurrentAndNewPos <- goDown newCurrentAndNewPos
            i <- i + 1
        newCurrentAndNewPos
    
    let goDownPath2 (currentPos : int * int) (grid : Dictionary<int*int, int>) limit round =
        let mutable newCurrentPos = currentPos
        let mutable i = 1
        while i <= 2 + 2 * round && grid.[newCurrentPos] < limit do
            newCurrentPos <- go movePositionDown newCurrentPos grid
            i <- i + 1
        newCurrentPos

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

    [<Literal>]
    let Input = 361527

    let firstWrittenValueBiggerThan limit =
        let grid = System.Collections.Generic.Dictionary<(int * int), int>()
        let mutable round = 0
        let mutable currentPos = ( 0, 0)
        grid.[currentPos] <- 1
        while grid.[currentPos] < limit do
            currentPos <- goRightPath2 currentPos grid limit round
            currentPos <- goUpPath2 currentPos grid limit round
            currentPos <- goLeftPath2 currentPos grid limit round
            currentPos <- goDownPath2 currentPos grid limit round
            round <- round + 1
        grid.[currentPos]

    let day3Part2 () =
        firstWrittenValueBiggerThan Input
