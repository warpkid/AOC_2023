open System.IO

let lines = File.ReadAllLines("./day2/input.txt")

let processString (game: int) (str: string) =
    game, str.Substring(str.IndexOf(":") + 2).Split(";")

type Ball =
    | Red
    | Green
    | Blue

let bag = [ Red, 12; Green, 13; Blue, 14 ] |> Map.ofList

let getGameResult (count: int) (colour: string) =
    printfn "Colour: %s Count: %i" colour count

    match colour.ToLower() with
    | "red" -> Red, count
    | "blue" -> Blue, count
    | "green" -> Green, count
    | _ -> failwith "Unknown Colour"

let getResultForSet (set: string) =
    set.Split(",")
    |> Array.map (fun r -> r.Trim().Split(" "))
    |> Array.map (fun a -> a[0], a[1])
    |> Array.map (fun (count, colour) -> getGameResult (int count) colour)

let maxSet (results: (Ball * int) array) =
    let state = [ Red, 0; Green, 0; Blue, 0 ] |> Map.ofList
    printfn "Summing Set"

    results
    |> Array.fold
        (fun acc r ->
            match r with
            | Red, count ->
                if count > (Map.find Red acc) then
                    Map.add Red count acc
                else
                    acc
            | Blue, count ->
                if count > (Map.find Blue acc) then
                    Map.add Blue count acc
                else
                    acc
            | Green, count ->
                if count > (Map.find Green acc) then
                    Map.add Green count acc
                else
                    acc)
        state

let results =
    lines
    |> Array.mapi (fun i l -> processString (i + 1) l)
    |> Array.map (fun (game, sets) ->
        printfn "Game Info %i %A" game sets
        let result = sets |> Array.map getResultForSet
        game, result)
    |> Array.map (fun (game, result) ->
        let squashed = result |> Array.collect id |> maxSet
        game, squashed)

let isPossible gameNumber bag result =
    printfn "Game: %i %A %A" gameNumber bag result
    gameNumber, Map.forall (fun key value -> value >= (Map.find key bag)) result


// valid games

results
|> Array.map (fun (gameNumber, results) -> isPossible gameNumber results bag)
|> Array.map (fun (g, r) ->
    printfn "Game: %i Valid: %b" g r
    g, r)
|> Array.sumBy (fun (g, v) -> if v = true then g else 0)

// powers

results
|> Array.map (fun (_, map) -> map |> Map.values |> Seq.reduce (fun v a -> v * a))
|> Array.sum


// guesses
// 2348
