module Day4

    open System
    open System.Collections.Generic
    
    let getLines path =
        System.IO.File.ReadAllLines path

    let isValidPhrase (s : String) =
        let words = s.Split ' '
        let wordsSet = new HashSet<string>(words)
        Array.length words = wordsSet.Count

    let countValidPassphrases () =
        let lines = getLines "Day4Input.txt"
        let validPhrases = Array.filter isValidPhrase lines
        Array.length validPhrases