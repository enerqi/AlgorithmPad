module Graphs.Tests

open System
open System.IO

open Chessie.ErrorHandling
open Graphs
open Fuchu
open FsUnit
open Swensen.Unquote
open Swensen.Unquote.Operators // [Under the hood extras]

module private TestUtils =
    let test_graph_file file_name = 
        let testFilesDir = __SOURCE_DIRECTORY__
        let path = Path.GetFullPath (Path.Combine(testFilesDir, file_name))
        if not <| File.Exists(path) then 
            failwith <| sprintf "test file %s not found" path
        path

    let load_undirected_test_graph = 
        let isDirected = false
        Generation.readGraphFromFile isDirected (test_graph_file "undirected_graph.txt") 

    let load_directed_test_graph = 
        let isDirected = true
        Generation.readGraphFromFile isDirected (test_graph_file "directed_graph.txt")

    let neighbours graph vertexIdNumber = 
        VertexId vertexIdNumber 
        |> Graph.vertexFromId graph 
        |> lift (fun v -> v.Neighbours)

    let neighbourIdNumbers graph vertexIdNumber = 
        trial {
            let! neighbourIds = neighbours graph vertexIdNumber
            let ns = neighbourIds 
                     |> Seq.map (fun vId -> vId.Id)
                     |> Array.ofSeq
                     |> Array.sort
            return ns            
        }
        
    let inline factorial (n: int): bigint = 
        if n < 0 then 
            failwith "negative faculty"  
                  
        let rec fact (n: bigint) (acc: bigint) =
            if n = 1I then 
                acc
            else 
                fact (n - 1I) (acc * n)

        fact (bigint(n)) 1I
        
    let ``n choose k combinations count`` n k = 
        (factorial n) / ((factorial k) * (factorial(n-k)))

    let n_choose_k n k = 
        let rec choose lo  =
            function
                |0 -> [[]]
                |i -> [for j=lo to (Array.length n)-1 do
                            for ks in choose (j+1) (i-1) do
                            yield n.[j] :: ks ]
            in choose 0 k  

    let expectOneFailedWith predicate result = 
        result |> failed |> should be True
        match result with 
        | Bad [e] -> predicate e
        | _ -> false        

    let isParsingFailure (gf: GraphFailure) = 
        match gf with 
        | ParsingFailure(_) -> true
        | _ -> false

    let isGraphAccessFailure (invalidVertexId: int) result = 
        match result with 
        | Bad [GraphAccessFailure (InvalidVertexId vid)] -> vid.Id = invalidVertexId
        | _ -> false
                      
 
open TestUtils

[<Tests>]
let graphTypeTests = 

    testList "Graph ADT" [
        testCase "vertex lookup" <| fun _ ->            
            trial {
                let! g = load_undirected_test_graph
                let! v1 = Graph.vertexFromId g (VertexId 1)
                return v1.Identifier
            }
            |> returnOrFail |> should equal (VertexId 1)

        testCase "vertex lookup with negative id returns GraphAccessFailure " <| fun _ ->           
            trial {
                let! g = load_undirected_test_graph
                return! Graph.vertexFromId g (VertexId -1)
            }
            |> isGraphAccessFailure -1 |> should be True

        testCase "vertex lookup with out of range id returns GraphAccessFailure " <| fun _ ->
            trial {
                let! g = load_undirected_test_graph
                return! Graph.vertexFromId g (VertexId 999)
            }
            |> isGraphAccessFailure 999 |> should be True
                
        testCase "vertices sequence is 1 based" <| fun _ ->
            trial {
                let! g = load_undirected_test_graph
                let hasVertexWithIdZero = 
                    Graph.verticesSeq g 
                    |> Array.ofSeq 
                    |> Array.exists (fun vertex -> vertex.Identifier.Id = 0) 
                return hasVertexWithIdZero
            }            
            |> returnOrFail |> should be False
    ]

[<Tests>]
let generationTests = 
    
    testList "Graph Generation" [
        testCase "Serialized header parsing" <| fun _ ->
            Generation.extractHeader "1 2" 
            |> returnOrFail |> should equal (1, 2)

        testCase "Header parsing returns ParsingFailure wrong number of inputs" <| fun _ ->
            Generation.extractHeader "1 2 3" |> expectOneFailedWith isParsingFailure |> should be True
            
        testCase "Header parsing returns ParsingFailure if cannot parse to numbers" <| fun _ ->
            Generation.extractHeader "a b" |> expectOneFailedWith isParsingFailure |> should be True

        testCase "Vertex Id pair parsing" <| fun _ ->
            Generation.extractVertexIdPair "1 2" 
            |> returnOrFail |> should equal (VertexId 1, VertexId 2)

        testCase "Vertex Id returns ParsingFailure wrong number of inputs" <| fun _ ->
            Generation.extractVertexIdPair "1 2 3" |> expectOneFailedWith isParsingFailure |> should be True

        testCase "Vertex Id pair parsing returns ParsingFailure if cannot parse to numbers" <| fun _ ->
            Generation.extractVertexIdPair "a b" |> expectOneFailedWith isParsingFailure |> should be True
                        
        testCase "read undirected graph works" <| fun _ ->
            let isDirected = false
            trial {
                let! g = Generation.readGraphFromFile isDirected (test_graph_file "undirected_graph.txt") 
                g.VerticesCount |> should equal 4
                g.EdgesCount |> should equal 5
                g.IsDirected |> should be False
                let adjacents = 
                    neighbourIdNumbers g >> returnOrFail
                adjacents 1 |> should equal [2; 4]
                adjacents 2 |> should equal [1; 3; 4]
                adjacents 3 |> should equal [2; 4]
                adjacents 4 |> should equal [1; 2; 3]
                return ()
            }
            |> returnOrFail

        testCase "read directed graph works" <| fun _ ->
            let isDirected = true
            trial {
                let! g = Generation.readGraphFromFile isDirected (test_graph_file "directed_graph.txt") 
                g.VerticesCount |> should equal 5
                g.EdgesCount |> should equal 8
                g.IsDirected |> should be True
                let adjacents = 
                    neighbourIdNumbers g >> returnOrFail
                adjacents 1 |> should equal [2]
                adjacents 2 |> should equal [5]
                adjacents 3 |> should equal [1; 4]
                adjacents 4 |> should equal [3]
                adjacents 5 |> should equal [1; 3; 4]
            }
            |> returnOrFail            
    ]   

[<Tests>]
let visualisationTests = 
    
    let perLineWhitespaceTrim (s: string) = 
        s.Split('\n') 
        |> Array.map (fun ss -> ss.Trim())
        |> String.concat "\n"
    
    let checkDotLanguage expectedDotString dotString = 
        perLineWhitespaceTrim dotString |> should equal (perLineWhitespaceTrim expectedDotString)                    

    testList "dot file language " [
        testCase "undirected graph" <| fun _ ->
            // match each line but strip leading/trailing whitespace
            let expected = "graph {
                1 -- 2
                1 -- 4
                2 -- 3
                2 -- 4
                3 -- 4
            }"
            trial {
                let! g = load_undirected_test_graph
                return Visualisation.toDotGraphDescriptionLanguage g
            }
            |> returnOrFail |> checkDotLanguage expected

        testCase "directed graph" <| fun _ ->
            let expected = "digraph {
                1 -> 2
                2 -> 5
                3 -> 1
                3 -> 4
                4 -> 3
                5 -> 1
                5 -> 3
                5 -> 4
            }"        
            trial {
                let! g = load_directed_test_graph
                return Visualisation.toDotGraphDescriptionLanguage g
            }
            |> returnOrFail |> checkDotLanguage expected
    ]


[<Tests>]
let algorithmTests = 

    let undirectedVertexCombinations (vertexIdNumbers: int array) = 
        let extract_pair list = 
            match list with 
            | a :: b :: _ -> (a, b)
            | _ -> failwith "logic error, not a pair"
        let pairs_directed = 
            n_choose_k vertexIdNumbers 2
            |> List.map extract_pair
        let pairs_reverse_direction = 
            pairs_directed
            |> List.map (fun (a, b) -> (b, a)) 
                                               
        pairs_directed @ pairs_reverse_direction
        |> List.map (fun (a, b) -> 
            (VertexId a, VertexId b))        
        |> Array.ofList

    let uniqueVertexIds g = 
        Graph.verticesSeq g
        |> Array.ofSeq 
        |> Array.map (fun v -> v.Identifier)

    testList "algorithms" [
        testCase "pathExists dfs v1 -> v2" <| fun _ ->
            trial {
                let! g = load_undirected_test_graph
                let idNums = uniqueVertexIds g |> Array.map (fun vId -> vId.Id)

                // This undirected graph has a path between all vertices
                for (v1, v2) in undirectedVertexCombinations idNums do                 
                    Algorithms.pathExists g v1 v2 |> returnOrFail |> should be True
            }
            |> returnOrFail
    ]
    

// Fuchu, FsUnit, Unquote and FsCheck Test library Examples /////////////////////////////
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
///////////////////////////////////////////////////////////////////////////////
    
[<EntryPoint>]
let main args =
    defaultMainThisAssembly args
    