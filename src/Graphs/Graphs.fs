namespace Graphs

open System
open System.Collections.Generic 
open System.IO

module Graphs = 

    type VertexId = 
        struct
            val Id: int
            new(id: int) = {Id = id}
        end

    type Vertex = {    
        Id: VertexId
        Neighbours: ResizeArray<VertexId>
    }  

    type Graph = {
        Vertices: Vertex array // mutable fixed sized .net type even though the ref is immutable
        IsDirected: bool
        VerticesCount: int
        EdgesCount: int
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
        | [|v1; v2|] -> (VertexId (int v1), VertexId (int v2))
        | _ -> failwith <| sprintf "Failed to extract pair from vertex pair string: %s" pairString

    let vertexFromId graph (v: VertexId) =
        graph.Vertices.[v.Id]

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
    let readGraph filePath isDirected = 
        let dataLines = File.ReadLines(filePath) 

        let header, edges = Seq.head dataLines, Seq.tail dataLines
        let verticesCount, edgesCount = extractHeader header
        let edgeVertexPairs = edges |> Seq.map extractVertexPair
    
        // The entry at index 0 will be ignored. Keeping it saves on offset calculations.
        // 1-based indices. F# inclusive [x..y]
        let verts = [|for vIndex in [0..verticesCount] do
                      yield { Id = VertexId vIndex;
                              Neighbours = new ResizeArray<VertexId>() } |]
            
        for (v1, v2) in edgeVertexPairs do 
            verts.[v1.Id].Neighbours.Add(v2)
            verts.[v2.Id].Neighbours.Add(v1)

        { VerticesCount = verticesCount; 
          IsDirected = isDirected;
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

    let reverseDirectedGraph graph = 
        if graph.IsDirected then
            let reverseGraph = {graph with Vertices = 
                                           [|for v in graph.Vertices do
                                             yield {Id = v.Id; 
                                                    Neighbours = new ResizeArray<VertexId>()}|]}
            
            for vertex in graph.Vertices do                                   
                for neighbourVertexId in vertex.Neighbours do 
                    if neighbourVertexId.Id < graph.VerticesCount then                                          
                        reverseGraph.Vertices.[neighbourVertexId.Id].Neighbours.Add(vertex.Id)                                           
            
            Some(reverseGraph)
        else
            None
           

    let isDAG directedGraph = 
        // Is it a directed *acyclic* graph?
        // - check if the graph has any strongly connected components with more than vertex
        // 
        // Note graphs with cycles cannot be linearly ordered
        // Theorem: any DAG can be linearly ordered
        // But...we need to find out if it is Acyclic.
        //
        // Another approach is:
        // Depth First Traversal can be used to detect cycle in a Graph. DFS for a connected graph produces a tree. 
        // There is a cycle in a graph only if there is a back edge present in the graph (self link or link to parent). 
        if not directedGraph.IsDirected then 
            false
        else 
            true

    let topologicalOrdering dag = 
        // source(s) at the start of the output, sink(s) at the end
        []

