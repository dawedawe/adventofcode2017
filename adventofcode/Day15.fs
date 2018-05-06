module Day15

let genAFactor : int64 = 16807L
let genBFactor : int64 = 48271L

let devisor : int64 = 2147483647L

let generate factor previous =
    (previous * factor) % devisor
    
let rec generatePart2 factor multipleOf previous =
    let r = (previous * factor) % devisor
    if r % multipleOf = 0L
    then r
    else generatePart2 factor multipleOf r

let generatorA = generate genAFactor

let generatorAPart2 = generatePart2 genAFactor 4L

let generatorBPart2 = generatePart2 genBFactor 8L

let generatorB = generate genBFactor

let lower16Bits (x : int64) =
    x &&& 0x000000000000ffffL

let judge rounds genA genB =
    let mutable genAValue = 873L
    let mutable genBValue = 583L
    let mutable matches = 0
    for _ in [1 .. rounds] do
        genAValue <- genA genAValue
        genBValue <- genB genBValue
        if (lower16Bits genAValue) = (lower16Bits genBValue)
        then matches <- matches + 1
    matches

let day15 () =
    judge 40000000 generatorA generatorB

    
let day15Part2 () =
    judge 5000000 generatorAPart2 generatorBPart2