module Server

open SAFE
open Saturn
open Shared


let wordleApi ctx = {
    getWord = fun () -> async { return "Hello" }
}

let webApp = Api.make wordleApi

let app = application {
    use_router webApp
    memory_cache
    use_static "public"
    use_gzip
}

[<EntryPoint>]
let main _ =
    run app
    0