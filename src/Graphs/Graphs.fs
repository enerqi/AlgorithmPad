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
        Identifier: VertexId
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
    let extractVertexIdPair (pairString: string) = 
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
        let edgeVertexPairs = edges |> Seq.map extractVertexIdPair
    
        // The entry at index 0 will be ignored. Keeping it saves on offset calculations.
        // 1-based indices. F# inclusive [x..y]
        let verts = [|for vIndex in [0..verticesCount] do
                      yield { Identifier = VertexId vIndex;
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
            if not (visitedSet.Contains(v.Identifier)) then
                componentGroups.Add(new ResizeArray<VertexId>())
                explore v.Identifier
                componentId <- componentId + 1

        componentGroups

    let reverseDirectedGraph graph = 
        if graph.IsDirected then
            let reverseGraph = {graph with Vertices = 
                                           [|for v in graph.Vertices do
                                             yield {Identifier = v.Identifier; 
                                                    Neighbours = new ResizeArray<VertexId>()}|]}
            
            for vertex in verticesSeq graph do                                   
                for neighbourVertexId in vertex.Neighbours do 
                    if neighbourVertexId.Id < graph.VerticesCount then // In case the graph has some invalid entries
                        reverseGraph.Vertices.[neighbourVertexId.Id].Neighbours.Add(vertex.Identifier)
            
            Some(reverseGraph)
        else
            None
           

    let isDAG graph = 
        // Is it a directed *acyclic* graph?
        // - Check if the graph has any strongly connected components with more than vertex
        //   This is best for finding all cycles in a graph, maybe overkill for finding if any cycles exist
        //
        // - Another approach is to detect any back edges in the DFS stack.
        // There is a cycle in a graph only if there is a back edge present in the graph (self link or link to parent). 
        // This involves checking whether a new node exists in the current dfs stack.
        // A simple scan of the stack is O(n) and doing this for each step in dfs is costly - O(n^2). O(|V|+|E|) for dfs component.
        // hashset - NO, we have a fixed number of vertices so can use a bitset
        // boolean array or bitset
        // Clearing and setting bits as the recursion goes keeps the complexity at O(|V|+|E|)
        //
        // https://stackoverflow.com/questions/261573/best-algorithm-for-detecting-cycles-in-a-directed-graph
        if graph.IsDirected then 
            let visitedSet = new HashSet<VertexId>() // or make a BitArray
            let dfsRecursionStackVertexIds = Array.create graph.VerticesCount false


            let rec explore (vertexId: VertexId) =  // returns true if a back-edge is found                
                            
                if dfsRecursionStackVertexIds.[vertexId.Id] then
                    // This is a back-edge pointing to vertex curretly being visited in the dfs recursion stack.
                    true
                else
                    // Not visited before (and so definitely also not in the dfs recursion stack)
                    visitedSet.Add(vertexId) |> ignore                
                    dfsRecursionStackVertexIds.[vertexId.Id] <- true

                    let vertex = vertexFromId graph vertexId
                    for neighbourVertexId in vertex.Neighbours do
                        if not (visitedSet.Contains(neighbourVertexId)) then 
                            explore vertexId 
                                                
                    dfsRecursionStackVertexIds.[vertexId.Id] <- false                    
                    false

            for vertex in verticesSeq graph do
                if not (visitedSet.Contains(vertex.Identifier)) then
                    if explore vertex.Identifier then 
                        true 
        else 
            false

    let topologicalOrdering dag = 
        // source(s) at the start of the output, sink(s) at the end
        []

