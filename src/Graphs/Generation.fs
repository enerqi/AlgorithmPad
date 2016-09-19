namespace Graphs

open System.IO

module Generation = 

    /// Take a pair string e.g. "1 2" and return a tuple of the integer values
    let extractHeader (header: string) = 
        header.Split() |> Array.map int |> fun xs -> 
            match xs with 
            | [|vC; eC|] -> (vC, eC)
            | _ -> failwith <| sprintf "Failed to extract header pair fom string: %s" header
  
    /// Take a pair string e.g. "1 2" and return a tuple of the VertexId values
    let extractVertexIdPair (pairString: string) = 
        match pairString.Split() with
        | [|v1; v2|] -> (VertexId (int v1), VertexId (int v2))
        | _ -> failwith <| sprintf "Failed to extract pair from vertex pair string: %s" pairString

    /// Parse a file and returns a Graph
    /// File format:
    ///
    /// Line 1: n (#vertices) m (#edges)
    ///         vertices use 1 based indices, 1 to n
    /// Next m lines = edge u v with id (>= 1 and <= n) - directed or undirected according to the problem.
    ///              | edge u v w - includes the weight 
    /// Graph should be simple - no self loops nor parallel edges.
    ///
    /// ## Parameters
    ///  - `file` - full (relative or absolute) string path to the file to open
    let readGraph filePath isDirected = 
        let dataLines = File.ReadLines(filePath) 

        let header, edges = Seq.head dataLines, Seq.tail dataLines
        let verticesCount, edgesCount = extractHeader header
        let edgeVertexPairs = edges |> Seq.map extractVertexIdPair
    
        // The entry at index 0 will be ignored. Keeping it saves on offset calculations.
        // 1-based indices. F# inclusive [x..y]
        let verts = [|for vIndex in [0..verticesCount] do
                      yield { Identifier = VertexId vIndex;
                              Neighbours = new ResizeArray<VertexId>() } |]
            
        for (v1, v2) in edgeVertexPairs do 
            // v1 -> v2
            verts.[v1.Id].Neighbours.Add(v2)
            
            // v2 -> v1 for bi-directionality
            if not isDirected then 
                verts.[v2.Id].Neighbours.Add(v1)

        { VerticesCount = verticesCount; 
          IsDirected = isDirected;
          EdgesCount = edgesCount;
          Vertices = verts }      
