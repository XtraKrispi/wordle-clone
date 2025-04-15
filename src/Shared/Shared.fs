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
    let EvaluateGuess actual guess = 
        guess 
        |> Seq.toList 
        |> List.zip (Seq.toList actual)
        |> List.map (fun (act, gue) -> 
        {
            character=gue
            result= 
            if act = gue 
            then CorrectPosition
            elif Seq.contains gue actual // correct this logic to account for all uses of letter being used
            then InWrongPosition
            else NotInWord
        })