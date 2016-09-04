namespace Graphs

open System
open System.Collections.Generic // ResizeArray
open System.IO

module Graphs = 

    type VertexId = VId of int  

    type Vertex = {    
        Id: VertexId
        Neighbours: ResizeArray<VertexId>
    }  

    type Graph = {
        VerticesCount: int
        EdgesCount: int
        Vertices: Vertex array // mutable fixed sized .net type even though the ref is immutable
    }

    /// Take a pair string e.g. "1 2" and return a tuple of the integer values
    let extractHeader (header: string) = 
        header.Split() |> Array.map int |> fun xs -> 
            match xs with 
            | [|vC; eC|] -> (vC, eC)
            | _ -> failwith <| sprintf "Failed to extract header pair fom string: %s" header
  
    /// Take a pair string e.g. "1 2" and return a tuple of the VertexId values
    let extractVertexPair (pairString: string) = 
        match pairString.Split() with
        | [|v1; v2|] -> (VId(int v1), VId(int v2))
        | _ -> failwith <| sprintf "Failed to extract pair from vertex pair string: %s" pairString

    let vertexFromId graph v =
        match v with 
        | VId id -> graph.Vertices.[id]

    let verticesSeq graph = 
        Seq.ofArray graph.Vertices |> Seq.skip 1         

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
    let readGraph filePath = 
        let dataLines = File.ReadLines(filePath) |> Array.ofSeq

            // head and rest of seq?
        let header, edges = dataLines.[0], dataLines.[1..]
        let verticesCount, edgesCount = extractHeader header
        let edgeVertexPairs = edges |> Array.map extractVertexPair
    
        // The entry at index 0 will be ignored. Keeping it saves on offset calculations.
        // 1-based indices. F# inclusive [x..y]
        let verts = [|for vIndex in [0..verticesCount] do
                      yield { Id = VId(vIndex)
                              Neighbours = new ResizeArray<VertexId>() } |]
            
        for (VId v1, VId v2) in edgeVertexPairs do 
            verts.[v1].Neighbours.Add((VId v2))
            verts.[v2].Neighbours.Add((VId v1))

        { VerticesCount = verticesCount; 
          EdgesCount = edgesCount;
          Vertices = verts }      

    let pathExists graph (v1: VertexId) (v2: VertexId) = 
        let visitedSet = new HashSet<VertexId>() 
        
        let rec explore (v: VertexId) =             
            visitedSet.Add(v) |> ignore            
            let vertex = vertexFromId graph v
            for neighbour in vertex.Neighbours do 
                if not (visitedSet.Contains(neighbour)) then 
                    explore neighbour // not tail recursive!
        
        explore v1
        visitedSet.Contains(v2)
            
    let connectedComponents graph =              
        let visitedSet = new HashSet<VertexId>()
        let mutable componentId = 0
        let componentGroups = new ResizeArray<ResizeArray<VertexId>>()
        
        let rec explore (v: VertexId) = 
            visitedSet.Add(v) |> ignore
            componentGroups.[componentId].Add(v)
            let vertex = vertexFromId graph v
            for neighbour in vertex.Neighbours do
                if not (visitedSet.Contains(neighbour)) then 
                    explore neighbour 

        for v in verticesSeq graph do 
            if not (visitedSet.Contains(v.Id)) then
                componentGroups.Add(new ResizeArray<VertexId>())
                explore v.Id
                componentId <- componentId + 1

        componentGroups
