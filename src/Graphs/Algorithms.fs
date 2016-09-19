namespace Graphs

open System.Collections.Generic 
open Nessos.Streams

module Algorithms = 

    let vertexFromId graph (v: VertexId) =
        graph.Vertices.[v.Id]

    let verticesSeq graph = 
        Seq.ofArray graph.Vertices |> Seq.skip 1         

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
        // Option 1:
        // - Check if the graph has any strongly connected components with more than vertex
        //   This is best for finding all cycles in a graph, maybe overkill for finding if any cycles exist
        // Option 2:
        // - Detect any back edges in the DFS stack.
        //   There is a cycle in a graph only if there is a back edge present in the graph (self link or link to parent). 
        //   This involves checking whether a new node exists in the current dfs stack.
        
        let checkDAG graph =
            let visitedSet = new HashSet<VertexId>() // or make a BitArray
            let dfsRecursionStackVertexIds: bool[] = Array.create (graph.VerticesCount + 1) false
            
            let rec explore (vertexId: VertexId) =  // returns true if a back-edge is found                
                dfsRecursionStackVertexIds.[vertexId.Id] || visitThisThenExploreChildren vertexId

            and visitThisThenExploreChildren (vertexId: VertexId) = 
                visitedSet.Add(vertexId) |> ignore                
                dfsRecursionStackVertexIds.[vertexId.Id] <- true

                let vertex = vertexFromId graph vertexId                    
                let mutable foundBackEdge = false // or use computation expressions http://tomasp.net/blog/imperative-i-return.aspx/
                for neighbourVertexId in vertex.Neighbours do
                    if not foundBackEdge && not (visitedSet.Contains(neighbourVertexId)) then 
                        foundBackEdge <- explore vertexId
                                                
                dfsRecursionStackVertexIds.[vertexId.Id] <- false                    
                foundBackEdge

            let mutable foundBackEdge = false
            for vertex in verticesSeq graph do
                if not foundBackEdge && not (visitedSet.Contains(vertex.Identifier)) then
                    foundBackEdge <- explore vertex.Identifier
            
            foundBackEdge

        graph.IsDirected && checkDAG graph

    let dfsPrePostOrderNumbers graph = 
        let visitedSet = new HashSet<VertexId>()
        let visitOrderNumbers = Array.create (graph.VerticesCount + 1) (0, 0)
        let mutable visitNumber = 0

        let rec explore (vertexId: VertexId) = 
            
            let preOrderVisitNumber = visitNumber
            visitedSet.Add(vertexId) |> ignore
            visitOrderNumbers.[vertexId.Id] <- (preOrderVisitNumber, 0)
            visitNumber <- visitNumber + 1

            let vertex = vertexFromId graph vertexId
            for neighbourVertexId in vertex.Neighbours do 
                if not (visitedSet.Contains(neighbourVertexId)) then 
                    explore neighbourVertexId

            let postOrderVisitNumber = visitNumber
            visitOrderNumbers.[vertexId.Id] <- (preOrderVisitNumber, postOrderVisitNumber)
            visitNumber <- visitNumber + 1

        for v in verticesSeq graph do
            if not (visitedSet.Contains(v.Identifier)) then
                explore v.Identifier 

        visitOrderNumbers
  
(*      
    let stronglyConnectedComponents (graph: Graph) = 
        
        let components = 
            // The vertex with the largest dfs post order number is in a source component
            // To get the sink component we need to reverse the graph and find the largest
            // post order number
            let processReverseGraph reverseGraph = 
                let visitedSet = new HashSet<VertexId>()

                let rec explore (vertexId: VertexId) = 
                    visitedSet.Add(vertexId) |> ignore
                    let vertex = vertexFromId reverseGraph vertexId
                    for neighbourVertexId in vertex.Neighbours do 
                        if not (visitedSet.Contains(neighbourVertexId)) then 
                            explore neighbourVertexId

                for v in verticesSeq reverseGraph do
                    if not (visitedSet.Contains(v.Identifier)) then
                        explore v.Identifier 

                []

            match reverseDirectedGraph graph with
            | None -> []
            | Some(rg) -> processReverseGraph rg
                       

        if graph.isDirected then
            Some(components)
        else 
            None
*)

    let topologicalOrdering dag = 
        // source(s) at the start of the output, sink(s) at the end
        []

    let edgesSet graph : Set<int * int> = 
                    
        let edgeFrom : (int -> VertexId -> int * int) = 
            if graph.IsDirected then
                (fun (vertexIndex: int) (neighbourVertexId: VertexId) ->
                    (vertexIndex, neighbourVertexId.Id))                    
            else
                let orderEdgeVertices (a, b) = 
                    if a <= b then (a, b) else (b, a)                
                (fun (vertexIndex: int) (neighbourVertexId: VertexId) ->
                    orderEdgeVertices (vertexIndex, neighbourVertexId.Id))                       

        let allEdgesFrom (vertexIndex: int, neighbours: seq<VertexId>) =
           seq { for v in neighbours do 
                 yield edgeFrom vertexIndex v }
           |> Stream.cast // seq to stream
            
        graph.Vertices
        |> Stream.ofArray
        |> Stream.mapi (fun index v -> (index, v.Neighbours))
        |> Stream.flatMap allEdgesFrom
        |> Stream.toSeq        
        |> Set.ofSeq
          