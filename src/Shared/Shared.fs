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

    let EvaluateGuess actual guess =
        let indexedWord = mapCharsToIndices actual
        guess
        |> Seq.toList
        |> List.fold (fun (index, remainingMap, guessAccumulator) c ->  
            match indexedWord |> Map.tryFind c with
            | Some indices when Set.contains index indices -> 
                (index + 1, 
                remainingMap |> Map.change c (fun existingValue -> 
                    match existingValue with
                    | Some indices when Set.contains index indices -> Some (Set.remove index indices)
                    | _ -> None), 
                guessAccumulator @ [{ character = c; result = CorrectPosition }])
            | Some indices -> (index + 1, remainingMap, guessAccumulator @ [{ character = c; result = InWrongPosition }])
            | None -> (index + 1, remainingMap, guessAccumulator @ [{ character = c; result = NotInWord }])
            ) (0, indexedWord, List.empty)
        // |> List.mapi (fun index gue -> 
        //     let indexList = indexedWord |> Map.tryFind gue.character

        //     {
        //     character = gue
        //     result = 
        //         match indexList with
        //         | Some indices when Set.contains index indices -> CorrectPosition
        //         | Some indices when Set.contains index indices |> not -> InWrongPosition
        //         | None -> NotInWord
        //     })

    // actual: [1, 4, 5]
    // guess: [1, 2, 4, 6]

    // intersection: [1, 4]
    // differenceActual: [1, 4, 5] - [1, 4] = [5]
    // diffrenceGuess: [1, 2, 4, 6] - [1, 4] = [2, 6]

    let EvaluateLetter (actual: Set<int>) (guess: Set<int>) : List<(int * LetterResult)> =
        let inCorrectPosition = Set.intersect actual guess
        let remainingActual = Set.difference actual inCorrectPosition |> Set.toList
        let remainingGuess = Set.difference guess inCorrectPosition |> Set.toList
        let combinedList = List.map Some remainingActual @ List.replicate (List.length remainingGuess - List.length remainingActual) None
        let zippedList = List.zip remainingGuess combinedList |> List.map (fun (g, a) -> 
            match a with
            | Some _ -> (g, InWrongPosition)
            | None -> (g, NotInWord))

        inCorrectPosition |> Set.toList |> List.map (fun i -> (i, CorrectPosition)) |> List.append zippedList |> List.sortBy fst