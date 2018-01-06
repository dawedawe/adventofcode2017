module Day15

let genAFactor : int64 = 16807L
let genBFactor : int64 = 48271L

let devisor : int64 = 2147483647L

let generate factor previous =
    (previous * factor) % devisor

let generatorA = generate genAFactor

let generatorB = generate genBFactor

let lower16Bits (x : int64) =
    x &&& 0x000000000000ffffL

let judge rounds =
    let mutable genAValue = 873L
    let mutable genBValue = 583L
    let mutable matches = 0
    for i in [1 .. rounds] do
        genAValue <- generatorA genAValue
        genBValue <- generatorB genBValue
        // printfn "%d %d" genAValue genBValue
        if (lower16Bits genAValue) = (lower16Bits genBValue)
        then matches <- matches + 1
    matches

let Day15 () =
    judge 40000000