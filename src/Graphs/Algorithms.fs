namespace Graphs

open System.Collections.Generic 
open System.Collections
open Nessos.Streams

module Algorithms =          

    open Chessie.ErrorHandling
    open Graph   

    type VisitedSet(graph: Graph) = 
        let flags = new BitArray(int32 graph.Vertices.Length)
        member this.Contains(vertexId: VertexId) = flags.Get(int32 vertexId.Id)
        member this.Insert(vertexId: VertexId) = flags.Set(int32 vertexId.Id, true)

            
    /// Return whether a path exists between two vertices on a graph
    let pathExists graph (v1: VertexId) (v2: VertexId) : GraphResult<bool> = 
        let visitedSet = VisitedSet(graph)
                
        let rec explore (v: VertexId): GraphResult<unit> =             
            visitedSet.Insert(v)
            trial {
                let! vertex = vertexFromId graph v            
                for neighbour in vertex.Neighbours do 
                    if not (visitedSet.Contains(neighbour)) then 
                        do! explore neighbour // not tail recursive!
            } 
        
        trial {
            do! explore v1
            return visitedSet.Contains(v2)
        }
                    
    /// Return the connected components of graph.
    let connectedComponents graph: GraphResult<ResizeArray<ResizeArray<VertexId>>> =              
        let visitedSet = VisitedSet(graph)
        let mutable componentId = 0
        let componentGroups = new ResizeArray<ResizeArray<VertexId>>()
        
        let rec explore (v: VertexId): GraphResult<unit> = 
            visitedSet.Insert(v)
            componentGroups.[componentId].Add(v)
            trial {
                let! vertex = vertexFromId graph v    
                for neighbour in vertex.Neighbours do
                    if not (visitedSet.Contains(neighbour)) then 
                        do! explore neighbour 
            }

        trial {
            for v in verticesSeq graph do 
                if not (visitedSet.Contains(v.Identifier)) then
                    componentGroups.Add(new ResizeArray<VertexId>())
                    do! explore v.Identifier
                    componentId <- componentId + 1
            
            return componentGroups
        }

    /// Return a new graph with all edges reversed. All edges point from their edge destinations to their sources.
    /// An undirected graph is returned unchanged.
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
            
            reverseGraph
        else
            graph
          
    /// Is a graph directed and acyclic
    let isDAG graph: GraphResult<bool> = 
        
        // - Detect any back edges in the Depth First Search stack.
        //   There is a cycle in a graph only if there is a back edge present in the graph (self link or link to parent). 
        //   This involves checking whether a new node exists in the current dfs stack.
        // Note, another option would be to check if there are any strongly connected components with more than one vertex        
        let checkDAG graph =
            let visitedSet = VisitedSet(graph) 
            let dfsRecursionStackVertexIds: bool[] = Array.create (graph.VerticesCount + 1) false
            
            let rec explore (vertexId: VertexId): GraphResult<bool> =  
                if dfsRecursionStackVertexIds.[vertexId.Id] then 
                    ok true
                else 
                    visitThisThenExploreChildren vertexId
                
            and visitThisThenExploreChildren (vertexId: VertexId): GraphResult<bool> = 
                visitedSet.Insert(vertexId)
                dfsRecursionStackVertexIds.[vertexId.Id] <- true // this vertex is on the stack
                let mutable foundBackEdge = false 

                trial {
                    let! vertex = vertexFromId graph vertexId
                    
                    for neighbourVertexId in vertex.Neighbours do
                        if not foundBackEdge && not (visitedSet.Contains(neighbourVertexId)) then 
                            let! backEdgeExists = explore vertexId
                            foundBackEdge <- backEdgeExists
                            
                    dfsRecursionStackVertexIds.[vertexId.Id] <- false // this vertex has finished being on the stack                    
                    return foundBackEdge
                }
            
            trial {
                let mutable foundBackEdge = false
                for vertex in verticesSeq graph do
                    if not foundBackEdge && not (visitedSet.Contains(vertex.Identifier)) then
                        let! backEdgeExists = explore vertex.Identifier
                        foundBackEdge <- backEdgeExists

                return foundBackEdge
            }

        if graph.IsDirected then
            checkDAG graph
        else
            ok false

    /// Return an array that can be indexed by VertexId.Id where each element contains the depth first search
    /// pre and post order numbers. 0 is the first visited vertex. With each new node visited the number is incremented.
    let dfsPrePostOrderNumbers graph: GraphResult<(int * int)[]> = 
        let visitedSet = VisitedSet(graph)
        let visitOrderNumbers = Array.create (graph.VerticesCount + 1) (0, 0)
        let mutable visitNumber = 0

        let rec explore (vertexId: VertexId): GraphResult<unit> = 
            
            let preOrderVisitNumber = visitNumber
            visitedSet.Insert(vertexId)
            visitOrderNumbers.[vertexId.Id] <- (preOrderVisitNumber, 0)
            visitNumber <- visitNumber + 1

            trial {
                let! vertex = vertexFromId graph vertexId
                for neighbourVertexId in vertex.Neighbours do 
                    if not (visitedSet.Contains(neighbourVertexId)) then 
                        do! explore neighbourVertexId

                let postOrderVisitNumber = visitNumber
                visitOrderNumbers.[vertexId.Id] <- (preOrderVisitNumber, postOrderVisitNumber)
                visitNumber <- visitNumber + 1    
            }

        trial {
            for v in verticesSeq graph do
                if not (visitedSet.Contains(v.Identifier)) then
                    do! explore v.Identifier 

            return visitOrderNumbers
        }
        
    /// Return the strongly connected components of the graph. 
    /// Components are strongly connected when there is a cycle amongst them meaning that they can all reach each other somehow.
    let stronglyConnectedComponents (graph: Graph) : GraphResult<ResizeArray<ResizeArray<VertexId>> option> = 
                
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

            let visitedSet = VisitedSet(graph)
            let componentGroups = new ResizeArray<ResizeArray<VertexId>>()
                
            let rec explore (vertexId: VertexId) (currentComponentGroup: ResizeArray<VertexId>) = 
                visitedSet.Insert(vertexId) |> ignore
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

        in reverseDirectedGraph graph |> Option.map findComponents

    
    /// Return the topological ordering (source vertices before sink vertices) of a directed acyclic graph
    let topologicalOrdering dag: GraphResult<VertexId []> = 

        trial {
            // Read off the reverse post order numbers 
            let! dfsOrderings = dfsPrePostOrderNumbers dag
            let orderedVertexIds = 
                dfsOrderings
                |> Stream.ofArray
                |> Stream.mapi (fun index prePostOrderNums -> (VertexId index, prePostOrderNums))
                |> Stream.skip 1 // ignore the 0 index vertex put in to make 1 based indexing easier                                                           
                |> Stream.sortBy (fun (_, (_, post)) -> -post) // Negative/reverse post 
                |> Stream.map (fun (vId, _) -> vId)
                |> Stream.toArray
            return orderedVertexIds
        }
            

    /// Return a set of all the edge (source, destination) vertex id pairs
    let edgesSet graph : Set<int * int> = 
            
        /// The function that given a source vertex index and a destination vertex id returns
        /// (source index, destination index) pair        
        let edgeFrom : (int -> VertexId -> int * int) = 
            if graph.IsDirected then
                (fun (vertexIndex: int) (neighbourVertexId: VertexId) ->
                    (vertexIndex, neighbourVertexId.Id))                    
            else
                // For undirected graphs we need to order the pairs so that the set
                // can remove duplicates - (1, 2) is really the same as (2, 1) when undirected
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
     
    
    /// Return the shortest path from a source vertex defined by the bfsData to a destination vertex
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
                | Uncoloured ->
                    q.Enqueue(neighbourId)
                    twoColourings.[neighbourIndex] <- requiredNeighboursColour
                | _ -> if neighbourColour <> requiredNeighboursColour then
                            failedTwoColouring <- true           

        not failedTwoColouring 
