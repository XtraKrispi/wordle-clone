module Index

open Elmish
open SAFE
open Shared

type Model = Int

type Msg = | NoOp

let todosApi = Api.makeProxy<IWordleApi> ()

let init () = 0, Cmd.none

let update msg model =
    match msg with
    | NoOp -> model, Cmd.none

open Feliz

let view model dispatch = Html.div "Hello Wordle!"