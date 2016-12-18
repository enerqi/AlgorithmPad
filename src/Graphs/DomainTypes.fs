namespace Graphs

[<AutoOpen>]
module DomainTypes = 
   
    open Chessie.ErrorHandling
    open System

    /// Main Graph datastructure type implemented with mutable Vertex structures
    type Graph = {
        Vertices: Vertex array 
        IsDirected: bool
        VerticesCount: int
        EdgesCount: int
    }
    /// Vertex (Node) in a graph using an adjacency list representation
    and Vertex = {    
        Identifier: VertexId
        Neighbours: ResizeArray<VertexId>
    } 
    /// Type safe wrapper of vertex integer id
    and VertexId = 
        struct
            val Id: int
            new(id: int) = {Id = id}
        end            

    /// Breadth first search results from a source vertex
    type ShortestPaths = {
        Source: VertexId
        ShortestPathDistances: Distance option []
        ShortestPathTree: VertexId option []
    }
    /// Type safe wrapper of distance integer values
    and Distance = 
        struct 
            val Distance: uint32
            new(d: uint32) = {Distance = d}
        end    

    type ShortestPathPriorityKey = 
        struct
            val Distance: Distance
            val Id: VertexId
            new(dist, id) = {
                Distance = dist
                Id = id
            }
        end
            
    type GraphFailure =
        | GraphAccessFailure of GraphAccessFailure
        | FileAccessFailure of Exception
        | ParsingFailure of string
        | VisualisationFailure of Exception
    and GraphAccessFailure = 
        | InvalidVertexId of VertexId

    /// Main functional style error handling result type used throughout the Graphs namespace
    type GraphResult<'TSuccess> = Result<'TSuccess, GraphFailure>  


    type internal TwoColouring = Red | Green | Uncoloured
   
