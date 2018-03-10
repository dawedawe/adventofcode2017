module Day4

    open System
    open System.Collections.Generic
    
    let getLines path =
        System.IO.File.ReadAllLines path

    let isValidPhrase (s : string) =
        let words = s.Split ' '
        let wordsSet = new HashSet<string>(words)
        Array.length words = wordsSet.Count

    let containsAnagram (s : string) =
        let words = s.Split ' '
        let wordArrays = Array.map (fun (w : string) -> Array.sort(w.ToCharArray())
                                                        |> Array.fold (fun x y -> x + string(y)) "") words
        let wordsSet = new HashSet<string>(wordArrays)
        Array.length words <> wordsSet.Count

    let isValidPhrasePart2 (s : string) =
        isValidPhrase s && not (containsAnagram s)

    let countValidPassphrases () =
        let lines = getLines "Day4Input.txt"
        let validPhrases = Array.filter isValidPhrase lines
        Array.length validPhrases

    let countValidPassphrasesPart2 () =
        let lines = getLines "Day4Input.txt"
        let validPhrases = Array.filter isValidPhrasePart2 lines
        Array.length validPhrases