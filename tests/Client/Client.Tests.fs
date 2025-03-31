module Client.Tests

open Fable.Mocha


let client =
    testList "Client" [
        testCase "Added todo"
        <| fun _ ->

            Expect.equal 1 1 "Todo should equal new todo"
    ]

let all =
    testList "All" [
#if FABLE_COMPILER // This preprocessor directive makes editor happy
        Shared.Tests.shared
#endif
        client
    ]

[<EntryPoint>]
let main _ = Mocha.runTests all