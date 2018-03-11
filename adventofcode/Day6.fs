module Day6

let distributeMemoryBlock (banks : int array) indexToDistribute =
    let mutable memoryToDistribute = banks.[indexToDistribute]
    let newConfig = Array.copy banks
    newConfig.[indexToDistribute] <- 0
    let mutable nextIndex = indexToDistribute
    while memoryToDistribute > 0 do
        nextIndex <- (nextIndex + 1) % newConfig.Length
        newConfig.[nextIndex] <- (newConfig.[nextIndex] + 1)
        memoryToDistribute <- (memoryToDistribute - 1)
    newConfig

let rec distributeMemoryHelper seenConfigs currentConfig cycles =
    let cycles' = cycles + 1
    let firstMax = Array.max currentConfig
    let firstMaxIndex = Array.findIndex (fun x -> x = firstMax) currentConfig
    let newConfig = distributeMemoryBlock currentConfig firstMaxIndex
    if not (Array.contains newConfig (Array.map fst seenConfigs))
    then
        let newSeenConfigs = Array.append seenConfigs [|(newConfig, cycles')|]
        distributeMemoryHelper newSeenConfigs newConfig (cycles')
    else
        let alreadySeen = Array.find (fun a -> fst a = newConfig) seenConfigs
        let loopSize = cycles' - (snd alreadySeen)
        cycles', loopSize

let distributeMemory startingConfig =
    let current = Array.copy startingConfig
    distributeMemoryHelper [|(startingConfig, 0)|] current 0

let day6 () =
    let startConfig = [|10; 3; 15; 10; 5; 15; 5; 15; 9; 2; 5; 8; 5; 2; 3; 6|]
    distributeMemory startConfig