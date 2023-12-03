open System.IO

let lines = File.ReadAllLines("./day2/input.txt")

let processString (game: int) (str: string) : int * string array =
    game, str.Substring(str.IndexOf(":") + 2).Split(";")

type Ball =
    | Red
    | Green
    | Blue

let bag = [ Red, 12; Green, 13; Blue, 14 ] |> Map.ofList

let getGameResult (count: int) (colour: string) : int * Ball =
    match colour.ToLower() with
    | "red" -> count, Red
    | "blue" -> count, Blue
    | "green" -> count, Green
    | _ -> failwith "Unknown Colour"

let getResultForSet (set: string) : (int * Ball) array =
    set.Split(",")
    |> Array.map (fun r ->
        let a = r.Trim().Split(" ")
        let count, colour = int a.[0], a.[1]
        getGameResult count colour)

let getMaximumBallsRequired (results: (int * Ball) array) : Map<Ball, int> =
    let state = [ Red, 0; Green, 0; Blue, 0 ] |> Map.ofList

    results
    |> Array.fold
        (fun acc (count, ball) ->
            let currentCount = Map.find ball acc
            if count > currentCount then Map.add ball count acc else acc)
        state

let results =
    lines
    |> Array.mapi (fun i l -> processString (i + 1) l)
    |> Array.map (fun (game, sets) ->
        let result =
            sets |> Array.map getResultForSet |> Array.collect id |> getMaximumBallsRequired

        game, result)

let isPossible gameNumber bag result =
    gameNumber, Map.forall (fun key value -> value >= (Map.find key bag)) result


// valid games

results
|> Array.map (fun (gameNumber, results) -> isPossible gameNumber results bag)
|> Array.sumBy (fun (game, isValid) -> if isValid then game else 0)

// powers

results
|> Array.map (fun (_, map) -> map |> Map.values |> Seq.reduce (fun v a -> v * a))
|> Array.sum
