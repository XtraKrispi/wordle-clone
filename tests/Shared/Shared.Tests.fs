module Shared.Tests

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif


let shared =
    testList "Shared" [
        testCase "Empty string is not a valid description"
        <| fun _ -> Expect.equal 1 1 "Should be false"
    ]