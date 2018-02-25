module Day21

open System

type Pixel = char
let Off = '.'
let On = '#'

type Pattern = Pixel [,]

type BrokenPattern = Pattern [,]

type Rule = Pattern * Pattern

let rotateCw90 (pattern : Pattern) =
    let rows = Array2D.length1 pattern
    let cols = Array2D.length2 pattern
    let pattern2 = Array2D.create cols rows Off
    for r in [0 .. (rows - 1)] do
        for c in [0 .. (cols - 1)] do
            let c' = (rows - 1) - r
            pattern2.[c, c'] <- pattern.[r, c]
    pattern2

let rotateCcw90 (pattern : Pattern) =
    let rows = Array2D.length1 pattern
    let cols = Array2D.length2 pattern
    let pattern2 = Array2D.create cols rows Off
    for r in [0 .. (rows - 1)] do
        for c in [0 .. (cols - 1)] do
            let r' = (cols - 1) - c
            pattern2.[r', r] <- pattern.[r, c]
    pattern2

let rotate180 = rotateCw90 >> rotateCw90

let flipH (pattern : Pattern) =
    let pattern2 = Array2D.copy pattern
    let rowsToFlip = Array2D.length1 pattern / 2
    for r in [0 .. (rowsToFlip - 1)] do
        let upperRowToSwap = pattern.[r, *]
        let r' = (Array2D.length1 pattern - 1) - r
        let lowerRowToSwap = pattern.[r', *]
        pattern2.[r', *] <- upperRowToSwap
        pattern2.[r, *] <- lowerRowToSwap
    pattern2

let flipV (pattern : Pattern) =
    let pattern2 = Array2D.copy pattern
    let colsToFlip = Array2D.length2 pattern / 2
    for c in [0 .. (colsToFlip - 1)] do
        let leftColToSwap = pattern.[*, c]
        let c' = (Array2D.length2 pattern - 1) - c
        let rightCOlToSwap = pattern.[*, c']
        pattern2.[*, c'] <- leftColToSwap
        pattern2.[*, c] <- rightCOlToSwap
    pattern2

let flipD (pattern : Pattern) =
    let pattern2 = Array2D.copy pattern
    for r in [0 .. (Array2D.length1 pattern - 2)] do
        for c in [1 .. (Array2D.length2 pattern - 1)] do
            let tmp = pattern.[r, c]
            pattern2.[r, c] <- pattern.[c, r]
            pattern2.[c, r] <- tmp
    pattern2

let flipHV = flipH >> flipV

let flipVH = flipV >> flipH

let rotateCw90flipV = rotateCw90 >> flipV

let rotateCw90flipH = rotateCw90 >> flipH

let createXbyX (pattern : Pattern) x r c =
    let newPattern = Array2D.create x x Off
    for r' in [0 .. (x - 1)] do
        for c' in [0 .. (x - 1)] do
            newPattern.[r', c'] <- pattern.[r + r', c + c']
    newPattern

let placeIntoGrid r c (p : Pattern) (grid : Pattern) =
    let dim = Array2D.length1 p
    for r' in [0 .. (dim - 1)] do
        for c' in [0 .. (dim - 1)] do
            grid.[r + r', c + c'] <- p.[r', c']
    grid

let listToGrid (patterns : Pattern []) =
    let elemDim = Array2D.length1 patterns.[0]
    let patternsDim = int (System.Math.Sqrt (float (Array.length patterns)))
    let dim = elemDim * patternsDim
    let mutable grid = Array2D.create dim dim Off
    let mutable skip = 0
    for r in [0 .. elemDim .. (dim - 1)] do
        let patternsOfRow = Array.take patternsDim patterns.[skip ..] 
        skip <- skip + patternsDim
        let mutable c = 0
        for p in patternsOfRow do
            grid <- placeIntoGrid r c p grid
            c <- c + elemDim
    grid

let breakUp (pattern : Pattern) : Pattern [] =
    let length = Array2D.length1 pattern
    let size = if length % 2 = 0
               then 2
               else 3 
    let mutable brokenPattern = [||]
    for r in [0 .. size .. (length - size)] do
        for c in [0 .. size .. length - size] do
            let newPattern = createXbyX pattern size r c
            brokenPattern <- Array.append brokenPattern (Array.singleton newPattern)
    brokenPattern

let parseRuleLine (line : string) : Rule =
    let index = line.IndexOf(" => ")
    let left = line.Substring(0, index)
    let right = line.Substring(index + 4)
    let left' = left.Split '/'
    let right' = right.Split '/'
    let left'' = Array.map (fun (s : string) -> s.ToCharArray()) left'
    let right'' = Array.map (fun (s : string) -> s.ToCharArray()) right'
    array2D left'', array2D right''

let isRuleCompatibleToPattern (rule : Rule) (pattern : Pattern) =
    let left = fst rule
    Array2D.length1 left = Array2D.length1 pattern

let isMatchingRule (rule : Pattern * Pattern) (pattern : Pattern) =
    if isRuleCompatibleToPattern rule pattern
    then
        let left = fst rule
        let mutable equal = true 
        for r in [0 .. (Array2D.length1 pattern - 1)] do
            let rowEqual = pattern.[r, *] = left.[r, *]
            equal <- equal && rowEqual
        equal
    else
        false

let findMatchingRule (rules : Rule []) transitions (pattern : Pattern) =
    let mutable matchingRule = None
    let mutable ruleIndex = 0
    while matchingRule.IsNone && ruleIndex < Array.length rules do
        let ruleToTry = rules.[ruleIndex]
        for t in transitions do
            let pattern' = t pattern
            if isMatchingRule ruleToTry pattern'
            then matchingRule <- Some ruleToTry
        ruleIndex <- ruleIndex + 1
    if matchingRule.IsNone
    then raise(Exception("no matching rule found"))
    else matchingRule.Value

let countPixels (pattern : Pattern) (pixel : Pixel) =
    let mutable count = 0
    for r in [0 .. (Array2D.length1 pattern - 1)] do
        let row = pattern.[r, *]
        let rowSum = Array.FindAll(row, (fun p -> p = pixel))
        count <- count + Array.length rowSum
    count

let day21 () =
    let input = System.IO.File.ReadAllLines "Day21Input.txt"
    let rules = Array.map parseRuleLine input
    let startPattern : Pattern = array2D [|
                                            [| '.'; '#'; '.' |];
                                            [| '.'; '.'; '#' |];
                                            [| '#'; '#'; '#' |];
                                         |]
    let transitions = [id; rotateCw90; rotateCcw90; rotate180; flipH; flipV; flipD;
                       rotateCw90flipV; rotateCw90flipH]
    let mutable currentPattern = startPattern
    for _ in [0 .. 4] do                   
        let brokenPatterns = breakUp currentPattern
        let mutable transitionedPatterns = [||]
        for p in brokenPatterns do 
            let matchingRule = findMatchingRule rules transitions p
            let transitionedPattern = snd matchingRule
            transitionedPatterns <- Array.append transitionedPatterns (Array.singleton (transitionedPattern))
            currentPattern <- listToGrid transitionedPatterns
    countPixels currentPattern On
