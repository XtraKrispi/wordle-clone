module Server

open SAFE
open Saturn
open Shared
open System.IO


let wordleApi ctx = {
    getWord =
        fun () -> async {
            let! words = File.ReadAllLinesAsync "wordle-La.txt" |> Async.AwaitTask
            let random = System.Random()
            let word = words.[random.Next(0, words.Length)]
            return word.ToLower()
        }
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