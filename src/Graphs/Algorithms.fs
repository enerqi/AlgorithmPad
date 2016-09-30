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
    
    let stronglyConnectedComponents (graph: Graph) : ResizeArray<ResizeArray<VertexId>> option = 
                
        let findComponents reverseGraph = 
            // The vertex with the largest dfs post order number is in a source component
            // To get the sink component we need to reverse the graph and find the largest
            // post order number
            // At that point exploring the graph (not reverse graph) in reverse post order
            // allows us to collect the strongly connected components
            let reversePostOrderVertices = 
                dfsPrePostOrderNumbers reverseGraph
                |> Stream.ofArray
                |> Stream.mapi (fun index prePostOrderNums -> (VertexId index, prePostOrderNums))
                |> Stream.skip 1 // ignore the 0 index vertex put in to make 1 based indexing easier                                                           
                |> Stream.sortBy (fun (_, (_, post)) -> -post) // Negative/reverse post 
                |> Stream.toArray

            let visitedSet = new HashSet<VertexId>()
            let componentGroups = new ResizeArray<ResizeArray<VertexId>>()
                
            let rec explore (vertexId: VertexId) (currentComponentGroup: ResizeArray<VertexId>) = 
                visitedSet.Add(vertexId) |> ignore
                currentComponentGroup.Add(vertexId)
                                                
                let vertex = vertexFromId graph vertexId
                for neighbourVertexId in vertex.Neighbours do 
                    if not (visitedSet.Contains(neighbourVertexId)) then 
                        explore neighbourVertexId currentComponentGroup

            for (vId, _) in reversePostOrderVertices do 
                if not (visitedSet.Contains(vId)) then 
                    componentGroups.Add(new ResizeArray<VertexId>())
                    let newestComponentGroup = componentGroups.[componentGroups.Count - 1]
                    explore vId newestComponentGroup
                
            componentGroups

        reverseDirectedGraph graph |> Option.map findComponents

    let topologicalOrdering dag = 
        // Read off the reverse post order numbers        
        dfsPrePostOrderNumbers dag
        |> Stream.ofArray
        |> Stream.mapi (fun index prePostOrderNums -> (VertexId index, prePostOrderNums))
        |> Stream.skip 1 // ignore the 0 index vertex put in to make 1 based indexing easier                                                           
        |> Stream.sortBy (fun (_, (_, post)) -> -post) // Negative/reverse post 
        |> Stream.map (fun (vId, _) -> vId)
        |> Stream.toArray
            

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
          

    let bfs graph (source: VertexId) : BFS = 
        
        let distances = Array.create (graph.VerticesCount + 1) None
        let pathTree = Array.create (graph.VerticesCount + 1) None
        distances.[source.Id] <- Some(Distance 0u)
        let q = new Queue<VertexId>(seq { yield source })  

        let stepDistance (dist: Distance option): Distance option = 
            match dist with 
            | Some(d) -> Some (Distance <| d.Distance + 1u)
            | None -> failwith "Algorithm error, distance to previous point in path should not be None."
            
        while q.Count > 0 do 
            let vId = q.Dequeue()
            let vertex = vertexFromId graph vId
            for neighbourId in vertex.Neighbours do 
                if distances.[neighbourId.Id].IsNone then
                    q.Enqueue(neighbourId)
                    let neighbourIndex = neighbourId.Id
                    let vIndex = vId.Id
                    distances.[neighbourIndex] <- stepDistance distances.[vIndex]
                    pathTree.[neighbourIndex] <- Some(vId)
                    
        {Source = source
         ShortestPathDistances = distances
         ShortestPathTree = pathTree}
     

    let shortestPath (bfsData: BFS) (v: VertexId): ResizeArray<VertexId> option = 

        if bfsData.ShortestPathTree.[v.Id].IsSome then
            let path = new ResizeArray<VertexId>()
            let mutable currentPathStep = v
            while currentPathStep <> bfsData.Source do 
                path.Add(currentPathStep)
                let pathIndex = currentPathStep.Id
                match bfsData.ShortestPathTree.[pathIndex] with 
                | Some(nextStep) -> currentPathStep <- nextStep
                | None -> failwith "BFS algorithm error, reached a dead end in the path"
            
            path.Reverse()
            Some(path)
        else
            None

    let isBipartite graph =
        // Its vertices can be split into two parts such that each edge of the
        // graph joins to vertices from different parts
        // In other words, a graph is bipartite if its vertices can be colored with two colors
        // (say, black and white) such that the endpoints of each edge have different colors. 

        // Run BFS and store the colour of each vertex as we go along, changing it on each edge
        // traversal. If the colour is already set and does not match the expected colour coming
        // out of another vertex then it isn't bipartite.
        
        let twoColourings = Array.create (graph.VerticesCount + 1) Uncoloured
        let firstVertexIndex = 1
        twoColourings.[firstVertexIndex] <- Red
        let q = Queue<VertexId>(seq { yield VertexId firstVertexIndex})

        let oppositeColour colour = 
            match colour with 
            | Red -> Green
            | Green -> Red
            | Uncoloured -> Uncoloured

        let mutable failedTwoColouring = false
        while q.Count > 0 && not failedTwoColouring do
                
            let vId = q.Dequeue()
            let vIndex = vId.Id
            let vColour = twoColourings.[vIndex]
            let requiredNeighboursColour = oppositeColour vColour
            let vertex = vertexFromId graph vId
                
            for neighbourId in vertex.Neighbours do 
                let neighbourIndex = neighbourId.Id
                let neighbourColour = twoColourings.[neighbourIndex]
                match neighbourColour with 
                | Blank ->
                    q.Enqueue(neighbourId)
                    twoColourings.[neighbourIndex] <- requiredNeighboursColour
                | _ -> if neighbourColour <> requiredNeighboursColour then
                            failedTwoColouring <- true           

        not failedTwoColouring 
