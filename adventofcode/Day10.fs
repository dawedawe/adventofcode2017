module Day10

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

let day10helper input lengths =
    let mutable nums = input
    let mutable pos = 0
    let mutable skipSize = 0
    for l in lengths do
        nums <- reverse nums l pos
        pos <- (pos + l + skipSize) % input.Length
        skipSize <- skipSize + 1
    nums 

let day10 () =
    let numbers = Array.init 256 id
    let lengths = [| 31; 2; 85; 1; 80; 109; 35; 63; 98; 255; 0; 13; 105; 254; 128; 33 |]
    let r = day10helper numbers lengths
    r.[0] * r.[1]