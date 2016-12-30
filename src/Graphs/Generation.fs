namespace Graphs

open System
open System.IO

module Generation = 

    open System.Collections.Generic
    open Chessie.ErrorHandling

    /// Transform a pair string of integers e.g. "1 2" and return a result of the tuple of the integer values
    let extractHeader (header: string) : GraphResult<int * int> = 
        trial {
            let! nums = tryF (fun _ -> header.Split((null: string[]), StringSplitOptions.RemoveEmptyEntries) |> Array.map int) (string >> ParsingFailure)            
            let pair = 
                match nums with 
                | [|vC; eC|] -> ok (vC, eC)
                | _ -> fail (ParsingFailure <| sprintf "Failed to extract header pair fom string: %s" header) 
            return! pair
        }

    /// Transform a string of two integers e.g. "1 2" into edge vertex ids. The ids must be >= 0.
    let extractEdge (pairString: string) : GraphResult<VertexId * VertexId> = 
        match pairString.Split((null: string[]), StringSplitOptions.RemoveEmptyEntries) with
        | [|v1; v2|] -> tryF (fun _ -> (VertexId (uint32 v1 |> int), VertexId (uint32 v2 |> int))) (string >> ParsingFailure)
        | _ -> fail (ParsingFailure <| sprintf "Failed to extract edge pair from vertex pair string: %s" pairString)
        
    /// Transform a string of three integers e.g. "1 2" or "1 2 3" into edge vertex ids plus an edge weight. The ids must be >= 0.
    let extractWeightedEdge (pairString: string) : GraphResult<VertexId * VertexId * Weight> = 
        match pairString.Split((null: string[]), StringSplitOptions.RemoveEmptyEntries) with
        | [|v1; v2; w|] -> tryF (fun _ -> (VertexId (uint32 v1 |> int), VertexId (uint32 v2 |> int), Weight (int w))) (string >> ParsingFailure)
        | _ -> fail (ParsingFailure <| sprintf "Failed to extract edge pair and weight from string: %s" pairString)
            

    /// Parse a line-oriented string representation to create a graph
    ///
    /// Line 1: n (#vertices) m (#edges)
    ///         vertices use 1 based indices, 1 to n
    /// Line (2 to m-1): edge u v with id (>= 1 and <= n) - directed or undirected according to the problem.
    ///                | edge u v w - includes the weight 
    /// For now the graph should be simple - no self loops nor parallel edges.
    let readGraph (isDirected: bool) (dataLines: seq<string>) : GraphResult<Graph> = 
        
        let nonBlankLines = dataLines |> Seq.filter (fun line -> line.Trim() <> String.Empty)
        let header, edges = Seq.head nonBlankLines, Seq.tail nonBlankLines

        // Might be better to let the user say whether or not the graph is weighted as this may fail to parse
        // but the graph may still be built up with other weighted entries
        let isWeightedGraph = edges |> Seq.head |> extractWeightedEdge |> (failed >> not)

        trial {
            // The entry at index 0 will be ignored. Keeping it saves on offset calculations.
            // 1-based indices. F# inclusive [x..y]
            let! (verticesCount, edgesCount) = extractHeader header
            let vertexArray = [|for vIndex in [0..verticesCount] do
                                yield { Identifier = VertexId vIndex;
                                        Neighbours = new ResizeArray<VertexId>()} |]   
            let weights = new Dictionary<_, _>()

            let addWeight : (VertexId -> VertexId -> Weight -> unit) = 
                if isDirected then
                    (fun v1 v2 w ->
                        weights.[(Source v1, Destination v2)] <- w
                        )
                else
                    (fun v1 v2 w ->
                        weights.[(Source v1, Destination v2)] <- w
                        weights.[(Source v2, Destination v1)] <- w
                        )                    

            let addEdge (verticesArray: Vertex[]) (v1: VertexId) (v2: VertexId) (w: Weight option) : GraphResult<unit> =
                trial {

                    // get Vertex via this function to check the Id is in bounds.
                    let! vertex1 = Graph.vertexFromArray verticesArray v1
                    vertex1.Neighbours.Add(v2)
                    // v2 -> v1 for bi-directionality
                    if not isDirected then 
                        let! vertex2 = Graph.vertexFromArray verticesArray v2
                        vertex2.Neighbours.Add(v1)

                    match w with 
                    | Some(weight) -> addWeight v1 v2 weight
                    | None -> ()
                }

            if isWeightedGraph then 
                let edgeData = edges |> Seq.map extractWeightedEdge
                for edge in edgeData do
                    match edge with 
                    | Ok ((v1, v2, w), _) -> do! addEdge vertexArray v1 v2 (Some w)
                    | Bad e -> printfn "Ignoring weighted vertex pair that could not parse: %A" e 
            else
                let edgeData = edges |> Seq.map extractEdge                          
                for edge in edgeData do
                    match edge with 
                    | Ok ((v1, v2), _) -> do! addEdge vertexArray v1 v2 None
                    | Bad e -> printfn "Ignoring unweighted vertex pair that could not parse: %A" e                            

            return 
                { VerticesCount = verticesCount
                  Weights = weights
                  IsDirected = isDirected
                  IsWeighted = isWeightedGraph
                  EdgesCount = edgesCount
                  Vertices = vertexArray }
        }    


    /// Parse a line-oriented string representation from a file to create a graph
    let readGraphFromFile (isDirected: bool) (filePath: string) : GraphResult<Graph> = 
        tryF (fun _ -> File.ReadLines(filePath)) FileAccessFailure
        >>= readGraph isDirected
