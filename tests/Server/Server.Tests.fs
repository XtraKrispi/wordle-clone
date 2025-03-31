module Server.Tests

open Expecto


let server =
    testList "Server" [
        testCase "Adding valid Todo"
        <| fun _ ->

            Expect.equal 1 1 ""
    ]

let all = testList "All" [ Shared.Tests.shared; server ]

[<EntryPoint>]
let main _ = runTestsWithCLIArgs [] [||] all