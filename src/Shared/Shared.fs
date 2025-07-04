namespace Shared

type LetterResult =
    | NotInWord
    | InWrongPosition
    | CorrectPosition

type Letter = {
    character: char
    result: LetterResult
}

type Guess = Letter list

type IWordleApi = { getWord: unit -> Async<string> }

module Logic =
    let mapCharsToIndices (word: string) : Map<char, Set<int>> =
        let updateMap (idx: int) (existingValue: Set<int> option) : Set<int> =
            existingValue
            |> Option.map (Set.add idx)
            |> Option.defaultValue (Set.singleton idx)

        word
        |> Seq.indexed
        |> Seq.fold (fun result (idx, char) -> result |> Map.change char (updateMap idx >> Some)) Map.empty

    // actual: [1, 4, 5]
    // guess: [1, 2, 4, 6]

    // intersection: [1, 4]
    // differenceActual: [1, 4, 5] - [1, 4] = [5]
    // diffrenceGuess: [1, 2, 4, 6] - [1, 4] = [2, 6]
    let EvaluateLetter (actual: Set<int>) (guess: Set<int>) : List<(int * LetterResult)> =
        let inCorrectPosition = Set.intersect actual guess
        let remainingActual = Set.difference actual inCorrectPosition |> Set.toList
        let remainingGuess = Set.difference guess inCorrectPosition |> Set.toList

        let combinedList =
            List.map Some remainingActual
            @ List.replicate (max (List.length remainingGuess - List.length remainingActual) 0) None

        let zippedList =
            if List.isEmpty remainingGuess then
                List.empty
            else
                List.zip remainingGuess combinedList
                |> List.map (fun (g, a) ->
                    match a with
                    | Some _ -> g, InWrongPosition
                    | None -> g, NotInWord)

        inCorrectPosition
        |> Set.toList
        |> List.map (fun i -> i, CorrectPosition)
        |> List.append zippedList
        |> List.sortBy fst

    let EvaluateGuess (actual: string) (guess: string) =
        let actualCharToIndicesMap = mapCharsToIndices (actual.ToLower())
        let guessCharToIndicesMap = mapCharsToIndices (guess.ToLower())

        guess.ToLower()
        |> Seq.toList
        |> List.distinct
        |> List.map (fun l ->
            EvaluateLetter
                (Map.tryFind l actualCharToIndicesMap |> Option.defaultValue Set.empty)
                (Map.find l guessCharToIndicesMap)
            |> List.map (fun (index, result) -> l, result, index))
        |> List.concat
        |> List.sortBy (fun (_, _, index) -> index)
        |> List.map (fun (l, result, _) -> { character = l; result = result })