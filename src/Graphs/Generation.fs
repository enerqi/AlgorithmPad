namespace Graphs

open System.IO

module Generation = 

    /// Transform a pair string of integers e.g. "1 2" and return a tuple of the integer values
    let extractHeader (header: string) : GraphResult<int* int> = 
        header.Split() |> Array.map int |> fun xs -> 
            match xs with 
            | [|vC; eC|] -> Ok (vC, eC)
            | _ -> Error <| sprintf "Failed to extract header pair fom string: %s" header
  
    /// Transform a pair string of integers e.g. "1 2" and return a tuple of the VertexId values
    let extractVertexIdPair (pairString: string) : GraphResult<VertexId * VertexId> = 
        match pairString.Split() with
        | [|v1; v2|] -> Ok (VertexId (int v1), VertexId (int v2))
        | _ -> Error <| sprintf "Failed to extract pair from vertex pair string: %s" pairString
            

    /// Parse a line-oriented string representation to create a graph
    /// File format:
    ///
    /// Line 1: n (#vertices) m (#edges)
    ///         vertices use 1 based indices, 1 to n
    /// Line (2 to m-1): edge u v with id (>= 1 and <= n) - directed or undirected according to the problem.
    ///                | edge u v w - includes the weight 
    /// For now the graph should be simple - no self loops nor parallel edges.
    let readGraph (dataLines: seq<string>) isDirected : GraphResult<Graph> = 
        
        let header, edges = Seq.head dataLines, Seq.tail dataLines
        
        // Mutable datastructures + functional style Result type error handling maybe a 
        // little painful to code...
        let verticesAndEdgesCount: GraphResult<int * int> = extractHeader header
        let vertCount headerData: GraphResult<int> = 
            Result.map (fun (vCount, _) -> vCount) headerData
        let edgeCount headerData: GraphResult<int> = 
            Result.map (fun (_, eCount) -> eCount) headerData 

        // verts and edges count
        // -> verts, edges, vertexArray
        // -> verts, edges, vertexArray, edgeVertexPairs
        // -> verts, edges, vertexArray, edgeVertexPairs ??? going through the edges...

        let makeVertexArray verticesCount : Vertex [] = 
            // The entry at index 0 will be ignored. Keeping it saves on offset calculations.
            // 1-based indices. F# inclusive [x..y]
            [|for vIndex in [0..verticesCount] do
                      yield { Identifier = VertexId vIndex;
                              Neighbours = new ResizeArray<VertexId>() } |]
        
        let verts: GraphResult<Vertex []> = 
            let countResult = vertCount verticesAndEdgesCount 
            Result.map makeVertexArray countResult
                   
        let edgeVertexPairs: seq<GraphResult<VertexId * VertexId>> = 
            edges |> Seq.map extractVertexIdPair    

        let addAllEdges vertices = 
            let addEdge (verticesArray: Vertex[]) (v1: VertexId) (v2: VertexId) = 
                verticesArray.[v1.Id].Neighbours.Add(v2)
                // v2 -> v1 for bi-directionality
                if not isDirected then 
                    verticesArray.[v2.Id].Neighbours.Add(v1)
            
            for startEndVertPairResult in edgeVertexPairs do
                 match startEndVertPairResult with 
                 | Ok (v1, v2) ->
                    addEdge vertices v1 v2                                
                 | Error e -> printfn "Ignoring vertex pair that could not parse: %s" e

        
        addAllEdges (if not error verts)   
        match (vertCount headerData, edgeCount headerData, verts) with 
        ...
   
        { VerticesCount = verticesCount; 
          IsDirected = isDirected;
          EdgesCount = edgesCount;
          Vertices = verts }
        

    /// Parse a line-oriented string representation from a file to create a graph
    let readGraphFromFile filePath isDirected = 
        let dataLines = File.ReadLines(filePath)
        readGraph dataLines isDirected 
