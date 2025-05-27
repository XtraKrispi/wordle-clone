module Index

open Elmish
open SAFE
open Shared

type Model = {
    word: RemoteData<Result<string, unit>>
    guesses: string list
}

type Msg =
    | NoOp
    | WordLoaded of string
    | WordFailed

let todosApi = Api.makeProxy<IWordleApi> ()

let init () : Model * Cmd<Msg> =
    {
        word = Loaded(Ok "hello")
        guesses = [ "apple"; "helno"; "train" ]
    },
    // Cmd.OfAsync.either todosApi.getWord () WordLoaded (fun _ -> WordFailed)
    Cmd.none

let update msg model =
    match msg with
    | NoOp -> model, Cmd.none
    | WordLoaded word -> { model with word = Loaded(Ok word) }, Cmd.none
    | WordFailed -> { model with word = Loaded(Error()) }, Cmd.none

open Feliz

let viewGrid model =
    let emptyBoxes =
        seq { 1..5 }
        |> Seq.map (fun _ -> Html.div [ prop.className "w-20 h-20 border border-black" ])

    let combinedList =
        List.map Some model.guesses
        @ List.replicate (max (6 - List.length model.guesses) 0) None
        |> List.map (fun x ->
            match x with
            | Some guess -> Html.text guess
            | None -> Html.div [ prop.className "flex gap-2"; prop.children emptyBoxes ])

    Html.div [ prop.className "flex flex-col gap-2"; prop.children combinedList ]

let view model dispatch =
    match model.word with
    | Loading _ -> Html.div [ prop.text "Loading..." ]
    | Loaded(Ok word) ->
        Html.div [
            prop.className "w-screen h-screen flex items-center justify-center"
            prop.children [ Html.div [ prop.children [ viewGrid model ] ] ]
        ]
    | Loaded(Error _) -> Html.div [ prop.text "Error loading word" ]
    | NotStarted -> Html.div [ prop.text "Not started" ]