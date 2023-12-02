open System.IO

let lines = File.ReadAllLines("./day1/input.txt")

let numbers =
    [ "one", 1
      "two", 2
      "three", 3
      "four", 4
      "five", 5
      "six", 6
      "seven", 7
      "eight", 8
      "nine", 9 ]

let charToInt (c: char) : int option =
    if System.Char.IsDigit(c) then
        Some(int (System.Char.GetNumericValue(c)))
    else
        None

let parseNumberFromLine (str: string) : int =
    str
    |> Seq.map charToInt
    |> Seq.choose id
    |> Seq.toList
    |> (fun a -> sprintf "%i%i" a[0] (List.last a))
    |> int

let rec parseNumberFromLinePart2 (str: string) (acc: int list) : int =
    match String.length str > 0 with
    | true ->
        match charToInt str[0] with
        | Some i -> parseNumberFromLinePart2 (str.Substring(1)) (i :: acc)
        | None ->
            match numbers |> List.filter (fun (numStr, _) -> str.StartsWith numStr) with
            | (_, num) :: _ -> parseNumberFromLinePart2 (str.Substring(1)) (num :: acc)
            | _ -> parseNumberFromLinePart2 (str.Substring(1)) acc
    | false -> (sprintf "%i%i" (List.last acc) acc[0]) |> int


lines |> Array.map (fun l -> parseNumberFromLinePart2 l []) |> Array.sum

// guesses 50193
// guesses 50170
// guesses 53340
