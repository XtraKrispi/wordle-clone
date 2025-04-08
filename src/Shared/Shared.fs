namespace Shared

type LetterResult =
    | NotInWord
    | InWrongPosition
    | CorrectPosition

type Letter = {
    character: char
    result: LetterResult
}

type Guess = { letters: Letter list }

type IWordleApi = { getWord: unit -> Async<string> }