module Index

open Elmish
open SAFE
open Shared
open Fable.Core.JsInterop


type GameState =
    | Guessing of
        {|
            word: string
            guesses: string list
            currentGuess: string
        |}
    | GameOver of {| 
            word: string
            guesses: string list 
        |}

type Model = {
    gameState: RemoteData<Result<GameState, unit>>
}

type Msg =
    | NoOp
    | WordLoaded of string
    | WordFailed
    | LetterGuessed of char
    | LetterDeleted
    | CommitGuess

let todosApi = Api.makeProxy<IWordleApi> ()

let init () : Model * Cmd<Msg> =
    {
        gameState = Loading None
    },
    Cmd.OfAsync.either todosApi.getWord () WordLoaded (fun _ -> WordFailed)

let update msg model =
    match msg with
    | NoOp -> model, Cmd.none
    | WordLoaded word -> { model with gameState = Loaded(Ok (Guessing  {| word = word; guesses = []; currentGuess = "" |})) }, Cmd.none
    | WordFailed -> { model with gameState = Loaded(Error()) }, Cmd.none
    | LetterGuessed c ->
        let newGameState =
            match model.gameState with
            | Loaded(Ok (Guessing st)) when st.currentGuess.Length < 5 ->
                Loaded(Ok (Guessing {|
                    word = st.word
                    guesses = st.guesses
                    currentGuess = $"{st.currentGuess}{c}"
                |}))
            | _ -> model.gameState

        { model with gameState = newGameState }, Cmd.none
    | LetterDeleted ->
        let newGameState =
            match model.gameState with
            | Loaded(Ok (Guessing st)) when st.currentGuess.Length > 0 ->
                Loaded(Ok (Guessing {|
                    word = st.word
                    guesses = st.guesses
                    currentGuess = $"{st.currentGuess[0 .. (st.currentGuess.Length - 2)]}"
                |}))
            | _ -> model.gameState

        { model with gameState = newGameState }, Cmd.none
    | CommitGuess ->
        let newGameState =
            match model.gameState with
            | Loaded(Ok (Guessing st)) when st.currentGuess.Length = 5 ->
                let newGuesses = st.guesses @ [ st.currentGuess ]

                if List.length st.guesses = 5 || st.currentGuess = st.word then
                    Loaded(Ok (GameOver {| word = st.word; guesses = newGuesses |}))

                else
                    Loaded(Ok (Guessing {|
                        word = st.word
                        guesses = newGuesses
                        currentGuess = ""
                    |}))
            | _ -> model.gameState


        { model with gameState = newGameState }, Cmd.none

open Feliz
open Browser
open System

let viewGuess renderColors word guess =
    let boxes =
        Logic.EvaluateGuess word guess
        |> List.map (fun l ->
            let className =
                if renderColors then
                    match l.result with
                    | NotInWord -> "bg-zinc-500 text-white"
                    | InWrongPosition -> "bg-yellow-500 text-white"
                    | CorrectPosition -> "bg-green-500 text-white"
                else
                    "border-2 border-gray-400"

            Html.div [
                prop.className (
                    "w-20 h-20 flex items-center justify-center font-extrabold text-4xl uppercase "
                    + className
                )
                prop.text (string l.character)
            ])

    Html.div [ prop.className "flex gap-2"; prop.children boxes ]

let viewGrid gameState =
    let numGuesses = 6

    let emptyBoxes =
        seq { 1..5 }
        |> Seq.map (fun _ -> Html.div [ prop.className "w-20 h-20 border-2 border-gray-400" ])

    // Pad the guess list with None values to ensure it has exactly numGuesses elements
    let combinedList =
        match gameState with
        | Guessing st -> [
            for g in st.guesses do
                viewGuess true st.word g

            viewGuess false st.word (sprintf "%-5s" st.currentGuess)

            for i in { 1 .. (max (numGuesses - List.length st.guesses - 1) 0) } do
                Html.div [ prop.className "flex gap-2"; prop.children emptyBoxes ]
          ]
        // List.map Some st.guesses
        // @ List.replicate (max (numGuesses - List.length st.guesses) 0) None
        // |> List.map (fun x ->
        //     match x with
        //     | Some guess -> viewGuess word guess
        //     | None -> Html.div [ prop.className "flex gap-2"; prop.children emptyBoxes ])
        | GameOver st -> [
            for g in st.guesses do
                viewGuess true st.word g


            for i in { 1 .. (max (numGuesses - List.length st.guesses) 0) } do
                Html.div [ prop.className "flex gap-2"; prop.children emptyBoxes ]
          ]

    Html.div [ prop.className "flex flex-col gap-2"; prop.children combinedList ]

type KeyType =
    | LetterKey of char
    | EnterKey
    | BackspaceKey

let viewKeyboard dispatch =
    let createButton (k: KeyType) =
        let baseClass = "h-20 bg-gray-400 rounded-md text-white font-bold uppercase"

        match k with
        | LetterKey c ->
            Html.button [
                prop.onClick (fun _ -> dispatch (LetterGuessed c))
                prop.className $"w-16 text-2xl {baseClass}"
                prop.text (string c)
            ]
        | EnterKey ->
            Html.button [
                prop.className $"w-24 text-xl {baseClass}"
                prop.text "Enter"
                prop.onClick (fun _ -> dispatch CommitGuess)
            ]
        | BackspaceKey ->
            Html.button [
                prop.onClick (fun _ -> dispatch LetterDeleted)
                prop.className $"w-24 text-xl {baseClass} flex justify-center items-center"
                prop.children [
                    Svg.svg [
                        svg.viewBox (0, 0, 24, 24)
                        svg.fill "none"
                        svg.strokeWidth 1.5
                        svg.stroke "currentColor"
                        svg.className "size-10"
                        svg.children [
                            Svg.path [
                                svg.strokeLineCap "round"
                                svg.strokeLineJoin "round"
                                svg.d
                                    "M12 9.75 14.25 12m0 0 2.25 2.25M14.25 12l2.25-2.25M14.25 12 12 14.25m-2.58 4.92-6.374-6.375a1.125 1.125 0 0 1 0-1.59L9.42 4.83c.21-.211.497-.33.795-.33H19.5a2.25 2.25 0 0 1 2.25 2.25v10.5a2.25 2.25 0 0 1-2.25 2.25h-9.284c-.298 0-.585-.119-.795-.33Z"
                            ]
                        ]
                    ]
                ]
            ]

    let firstRow = "qwertyuiop"
    let secondRow = "asdfghjkl"
    let thirdRow = "zxcvbnm"

    Html.div [
        prop.className "flex flex-col gap-2"
        prop.children [
            Html.div [
                prop.className "flex justify-center gap-2"
                firstRow |> Seq.map (LetterKey >> createButton) |> prop.children
            ]
            Html.div [
                prop.className "flex justify-center gap-2"
                secondRow |> Seq.map (LetterKey >> createButton) |> prop.children
            ]
            Html.div [
                prop.className "flex justify-center gap-2"
                prop.children [
                    createButton EnterKey
                    for c in thirdRow do
                        createButton (LetterKey c)
                    createButton BackspaceKey
                ]
            ]
        ]
    ]
    

let viewGame gameState dispatch =
    Html.div [
        prop.className "flex flex-col items-center gap-48"
        prop.children [ viewGrid gameState; viewKeyboard dispatch ]
    ]

let view model dispatch =
    match model.gameState with
    | Loading _ -> Html.div [ prop.text "Loading..." ]
    | Loaded(Ok st) ->
        Html.div [
            prop.className "w-screen h-screen flex items-center justify-center"
            prop.children [ viewGame st dispatch ]
        ]
    | Loaded(Error _) -> Html.div [ prop.text "Error loading word" ]
    | NotStarted -> Html.div [ prop.text "Not started" ]

let subscription model =
    let keyDownHandler dispatch =
        let handler e =
            match e?keyCode with
            | 13 -> dispatch CommitGuess
            | 8 -> dispatch LetterDeleted
            | k when k >= 97 && k <= 122 || k >= 65 && k <= 90 -> dispatch (LetterGuessed(char k))
            | _ -> ()

        document.addEventListener ("keydown", handler)

        { new IDisposable with
            member _.Dispose() =
                document.removeEventListener ("keydown", handler)
        }

    let subscribe model = [ [ "keyDown" ], keyDownHandler ]

    match model.gameState with
    | Loaded(Ok (Guessing _)) -> subscribe model
    | _ -> Sub.none