module Day20

open System
open System.Text.RegularExpressions

type Particle = {
    id : int
    p : int * int * int
    v : int * int * int
    a : int * int * int
}

let manhattanDistance (x, y, z) =
    abs x + abs y + abs z

let tick part =
    let (xV, yV, zV) = part.v
    let (xA, yA, zA) = part.a
    let (xV', yV', zV') = (xV + xA, yV + yA, zV + zA)
    let (xP, yP, zP) = part.p
    let newP = xP + xV', yP + yV', zP + zV'
    let newV = (xV', yV', zV')
    { part with p = newP; v = newV }

let parseTuple (t : string) =
    let t' = t.Replace("<", "").Replace(">", "")
    let ints = t'.Split [|','|]
    Int32.Parse(ints.[0]), Int32.Parse(ints.[1]), Int32.Parse(ints.[2])

let parseLine (line : string * int) =
    let matches = Regex.Matches(fst line, @"(<-*\d+\,-*\d+\,-*\d+>)")
    let p = matches.[0].Groups.[0].Value |> parseTuple
    let v = matches.[1].Groups.[0].Value |> parseTuple
    let a = matches.[2].Groups.[0].Value |> parseTuple
    { id = snd line; p = p; v = v; a = a }

let smallestIndexesBy (f : Particle -> int) particles =
    let sorted = Array.sortBy f particles
    let min = f sorted.[0]
    Array.takeWhile (fun p -> f p = min) sorted

let getNearest (particles : Particle []) =
    smallestIndexesBy (fun p -> manhattanDistance p.a) particles
    |> smallestIndexesBy (fun p -> manhattanDistance p.v)
    |> fun a -> a.[0].id

[<Literal>]
let InputFile = "Day20Input.txt"

let getParticles () =
    let input = System.IO.File.ReadAllLines InputFile
    let inputIndexed = Array.zip input [|0 .. (input.Length - 1)|]
    Array.map parseLine inputIndexed

let day20 () =
    let particles = getParticles()
    getNearest particles

let day20Part2 () =
    let mutable particles = getParticles()
    for _ in [1 .. 1000] do
        particles <- Array.map tick particles
        let particleGroups = Array.groupBy (fun p -> p.p) particles
        let singletonGroups = Array.filter (fun g -> (Array.length (snd g)) = 1) particleGroups
        particles <- Array.map (fun (g : (int*int*int) * Particle []) -> (snd g).[0]) singletonGroups
    particles.Length