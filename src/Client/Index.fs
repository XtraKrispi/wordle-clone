module Index

open Elmish
open SAFE
open Shared

type Model = {word: RemoteData<Result<string, unit>>}

type Msg =
    | NoOp
    | WordLoaded of string
    | WordFailed

let todosApi = Api.makeProxy<IWordleApi> ()

let init (): Model * Cmd<Msg> = { word=Loading None}, Cmd.OfAsync.either todosApi.getWord () (_.word >> WordLoaded) (fun ex -> WordFailed)

let update msg model =
    match msg with
    | NoOp -> model, Cmd.none
    | WordLoaded word -> { model with word = Loaded (Ok word) }, Cmd.none
    | WordFailed -> { model with word = Loaded (Error ()) }, Cmd.none

open Feliz

let view model dispatch =
    match model.word with
    | Loading _ -> Html.div [ prop.text "Loading..." ]
    | Loaded (Ok word) -> Html.div [ prop.text word ]
    | Loaded (Error _) -> Html.div [ prop.text "Error loading word" ]
    | NotStarted -> Html.div [ prop.text "Not started" ]