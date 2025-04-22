module Shared.Tests

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

open Shared.Logic

let shared =
    testList "Shared" [
        testCase "Guess should be correct"
        <| fun _ -> let actual = "hello"
                    let guess = "hello"
                    let result = EvaluateGuess actual guess
                    Expect.equal result [
                        {character='h'; result=CorrectPosition}
                        {character='e'; result=CorrectPosition}
                        {character='l'; result=CorrectPosition}
                        {character='l'; result=CorrectPosition}
                        {character='o'; result=CorrectPosition}
                    ] "why..."

        testCase "Guess should be incorrect - letter not in word"
        <| fun _ -> let actual = "hello"
                    let guess = "helno"
                    let result = EvaluateGuess actual guess
                    Expect.equal result [
                        {character='h'; result=CorrectPosition}
                        {character='e'; result=CorrectPosition}
                        {character='l'; result=CorrectPosition}
                        {character='n'; result=NotInWord}
                        {character='o'; result=CorrectPosition}
                    ] "why..."
        
        testCase "Guess should be incorrect - letter in incorrect position"
        <| fun _ -> let actual = "hello"
                    let guess = "helol"
                    let result = EvaluateGuess actual guess
                    Expect.equal result [
                        {character='h'; result=CorrectPosition}
                        {character='e'; result=CorrectPosition}
                        {character='l'; result=CorrectPosition}
                        {character='o'; result=InWrongPosition}
                        {character='l'; result=InWrongPosition}
                    ]  "why..."
    ]