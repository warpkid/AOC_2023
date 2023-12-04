open System.IO

let lines = File.ReadAllLines("./day3/input.txt")

let inputData = lines |> List.ofArray |> List.map (fun l -> seq l |> Seq.toList)

let isNumeric (char: char) : bool = System.Char.IsDigit(char)

let isSymbol (char: char) : bool =
    match System.Char.IsDigit(char), char = '.' with
    | true, false
    | false, true -> false
    | _ -> true

type State =
    { Numbers: int list
      NumberBuilder: string
      IsAdjacent: bool
      Counter: int }

let engineParts =
    { Numbers = []
      NumberBuilder = ""
      IsAdjacent = false
      Counter = 0 }

let processState (state: State) =
    if state.NumberBuilder.Length > 0 then
        { state with
            Numbers = state.Numbers @ [ (int state.NumberBuilder) ] }
    else
        state

inputData
|> List.mapi (fun i l ->
    l
    |> Seq.fold
        (fun state c ->
            let state =
                match isNumeric c with
                | true ->
                    let adjChecks = []

                    match i, state.Counter with
                    | i, c when i = 0 && c = 0 -> ()
                    | i, c when i = 0 && c > 0 -> ()
                    | _ -> ()

                    let adj =
                        inputData[i - 1][state.Counter] |> isSymbol
                        || inputData[i + 1][state.Counter] |> isSymbol
                        || l[state.Counter - 1] |> isSymbol
                        || l[state.Counter + 1] |> isSymbol

                    { state with
                        NumberBuilder = state.NumberBuilder + string c
                        IsAdjacent = adj
                        Counter = state.Counter + 1 }
                | false ->
                    processState
                        { state with
                            Counter = state.Counter + 1 }

            state)
        engineParts)
