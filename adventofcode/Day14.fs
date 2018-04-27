module Day14
open System

let replace (oldNs : int array) pos (replacement : int array) =
    let mutable repPos = 0 
    let mutable oldPos = pos
    while repPos < Array.length replacement do
        oldNs.[oldPos] <- replacement.[repPos]
        repPos <- repPos + 1
        oldPos <- (oldPos + 1) % oldNs.Length
    oldNs

let calcEndPos pos l length =
    if l = 0
    then pos
    else
        let endPos = pos + l - 1 
        if endPos >= length
        then length - 1
        else endPos

let reverse (ns : int array) (l : int) (pos : int) =
    if l > 1
    then
        let endPos = calcEndPos pos l ns.Length
        let mutable ns' = ns.[pos .. endPos]
        if l > Array.length ns'
        then let l' = l - Array.length ns'
             let ns'' = Array.take l' ns
             ns' <- Array.append ns' ns''
        ns' <- Array.rev ns'
        replace ns pos ns'
    else
        ns

let sparseHash input lengths =
    let mutable nums = input
    let mutable pos = 0
    let mutable skipSize = 0
    for i in [1..64] do
        for l in lengths do
            nums <- reverse nums l pos
            pos <- (pos + l + skipSize) % input.Length
            skipSize <- skipSize + 1
    nums

let denseHash (sparse : int array) =
    let mutable numbers = [||]
    for i in [0 .. 15] do
        let lowerBound = i * 16
        let upperBound = lowerBound + 15
        let n = Array.fold (^^^) 0 sparse.[lowerBound .. upperBound]    
        numbers <- Array.append numbers [|n|]
    numbers

let prepareInput (s : string) =
    let suffix = [| 17; 31; 73; 47; 23 |]
    let mutable asciiCodes = [||]
    for c in s.ToCharArray() do
        asciiCodes <- Array.append asciiCodes [|int(c)|]
    Array.append asciiCodes suffix

let hash (input : string) =
    let numbers = Array.init 256 id
    let lengths = prepareInput input
    sparseHash numbers lengths
    |> denseHash
    // |> Array.map (fun n -> sprintf "%02x" n)
    // |> Array.fold (+) ""

let bitPattern n = 
    let mutable bits = [||]
    for s in [0 .. 7] do
        let bitSet = (n >>> s) % 2
        bits <- Array.append [|bitSet|] bits
    bits

[<Literal>]
let Input = "jxqlasbh"

let day14 () =
    let mutable sum = 0
    for r in [0 .. 127] do
        let rowInput = Input + "-" + string(r)
        let h = hash rowInput
        let bits = Array.map bitPattern h |> Array.collect id
        let bitCount = Array.filter (fun b -> b = 1) bits
        sum <- sum + bitCount.Length
    sum

let findStartPos matrix =
    let mutable startPos = Option.None
    let mutable r = 0
    let mutable c = 0
    while r < Array2D.length1 matrix && Option.isNone startPos do
        while c < Array2D.length2 matrix && Option.isNone startPos do
            if matrix.[r, c] = "#"
            then startPos <- Option.Some (r, c)
            c <- c + 1
        c <- 0
        r <- r + 1
    startPos

let adjacentRegionPositions (matrix : string [,]) startPos =
    let legalPos (x, y) =
        x >= 0 && x < Array2D.length1 matrix
            && y >= 0 && y < Array2D.length2 matrix
             && matrix.[x, y] = "#"
    let r, c = startPos
    let upper = r - 1, c
    let lower = r + 1, c
    let left = r, c - 1
    let right = r, c + 1
    Array.filter legalPos [|upper; lower; left; right|]

let rec setRegion (matrix : string [,]) regionNumber startPos =
    matrix.[fst startPos, snd startPos] <- string(regionNumber)
    let adjacentRegPositions = adjacentRegionPositions matrix startPos
    for p in adjacentRegPositions do
        setRegion matrix regionNumber p

let setRegions (matrix : string [,]) =
    let mutable startPos = findStartPos matrix
    let mutable regionNumber = 0
    while Option.isSome startPos do
        regionNumber <- regionNumber + 1
        setRegion matrix regionNumber startPos.Value
        startPos <- findStartPos matrix
    regionNumber

let day14Part2 () =
    let matrix = Array2D.create 128 128 "."
    for r in [0 .. 127] do
        let rowInput = Input + "-" + string(r)
        let h = hash rowInput
        let bits = Array.map bitPattern h |> Array.collect id
        for c in [0 .. 127] do
            if bits.[c] = 1
            then matrix.[r, c] <- "#"
    let lastRegionNumber = setRegions matrix
    lastRegionNumber
    