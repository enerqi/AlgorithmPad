module Graphs.Tests

open Graphs
open Fuchu
open FsUnit
open Swensen.Unquote
open Swensen.Unquote.Operators // [Under the hood extras]

let fsCheckConfigOverride = { FsCheck.Config.Default with MaxTest = 10000 }

let testExamples = 
    testList "test list example" [
        
        testCase "Fuchu's basic assertion mechanism" <| fun _ ->
            Assert.Equal("1 is 1", 1, 1)                   

        testCase "FsUnit's expanded assertion library" <| fun _ ->
            1 |> should equal 1 
            1 |> should not' (equal 2)
            10.1 |> should (equalWithin 0.1) 10.11
            10.1 |> should not' ((equalWithin 0.001) 10.11)
            "ships" |> should startWith "sh"
            "ships" |> should not' (startWith "ss")
            "ships" |> should endWith "ps"
            "ships" |> should not' (endWith "ss")
            [1] |> should contain 1
            [] |> should not' (contain 1)
            [1..4] |> should haveLength 4
            //(fun () -> failwith "BOOM!" |> ignore) |> should throw typeof<System.Exception>
            true |> should be True
            false |> should not' (be True)
            "" |> should be EmptyString
            "" |> should be NullOrEmptyString
            null |> should be NullOrEmptyString
            null |> should be Null
            let anObj = "hi"
            let otherObj = "ho"
            anObj |> should not' (be Null)
            anObj |> should be (sameAs anObj)
            anObj |> should not' (be sameAs otherObj)
            11 |> should be (greaterThan 10)
            9 |> should not' (be greaterThan 10)
            11 |> should be (greaterThanOrEqualTo 10)
            9 |> should not' (be greaterThanOrEqualTo 10)
            10 |> should be (lessThan 11)
            10 |> should not' (be lessThan 9)
            10.0 |> should be (lessThanOrEqualTo 10.1)
            10 |> should not' (be lessThanOrEqualTo 9)
            0.0 |> should be ofExactType<float>
            1 |> should not' (be ofExactType<obj>)
            [] |> should be Empty // NUnit only
            [1] |> should not' (be Empty) // NUnit only
            "test" |> should be instanceOfType<string> // Currently, NUnit only and requires version 1.0.1.0+
            "test" |> should not' (be instanceOfType<int>) // Currently, NUnit only and requires version 1.0.1.0+
            2.0 |> should not' (be NaN) // Currently, NUnit only and requires version 1.0.1.0+
            [1;2;3] |> should be unique // Currently, NUnit only and requires version 1.0.1.0+

        testCase "Unquote step-by-step expression evaluation" <| fun _ ->
            test <@ (1+2)/3 = 1 @>
            // Some sugared quoted expressions <, > etc. with '!' suffix.
            true =! true
            1 <! 2
            2 >! 1
            4 <=! 4
            5 >=! 5
            "a" <>! "b"

        testCase "Unquote decompiling, evaluating and reducing of quotation expressions" <| fun _ ->
            //open Swensen.Unquote.Operators
            unquote <@ (1+2)/3 @> |> ignore
            decompile <@ (1+2)/3 @> |> ignore
            eval <@ "Hello World".Length + 20 @> |> ignore
            evalRaw<int> <@@ "Hello World".Length + 20 @@> |> ignore
            <@ (1+2)/3 @> |> reduce |> decompile |> ignore
            <@ (1+2)/3 @> |> reduceFully |> List.map decompile |> ignore
            <@ (1+2)/3 @> |> isReduced |> ignore
                    
        testProperty "FsCheck via Fuchu" <| 
            fun a b ->
                a + b = b + a
            
        testPropertyWithConfig fsCheckConfigOverride "Product is distributive over addition " <| 
            fun a b c ->
              a * (b + c) = a * b + a * c

    ]

[<EntryPoint>]
let main args =
    run testExamples