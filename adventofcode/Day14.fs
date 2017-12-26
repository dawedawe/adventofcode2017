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

let day14 () =
    let input = "jxqlasbh"
    let mutable sum = 0
    for r in [0 .. 127] do
        let rowInput = input + "-" + string(r)
        let h = hash rowInput
        let bits = Array.map bitPattern h |> Array.collect id
        let bitCount = Array.filter (fun b -> b = 1) bits
        sum <- sum + bitCount.Length
    sum
        
        
