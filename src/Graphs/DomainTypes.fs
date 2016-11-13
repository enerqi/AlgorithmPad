namespace Graphs

[<AutoOpen>]
module DomainTypes = 
   
    open Chessie.ErrorHandling
    open System

    /// Type safe wrapper of vertex integer id
    type VertexId = 
        struct
            val Id: int
            new(id: int) = {Id = id}
        end

    /// Vertex (Node) in a graph using an adjacency list representation
    type Vertex = {    
        Identifier: VertexId
        Neighbours: ResizeArray<VertexId>
    }  

    /// Main Graph datastructure type implemented with mutable Vertex structures
    type Graph = {
        Vertices: Vertex array 
        IsDirected: bool
        VerticesCount: int
        EdgesCount: int
    }

    /// Type safe wrapper of distance integer values
    type Distance = 
        struct 
            val Distance: uint32
            new(d: uint32) = {Distance = d}
        end

    /// Breadth first search results from a source vertex
    type BFS = {
        Source: VertexId
        ShortestPathDistances: Distance option []
        ShortestPathTree: VertexId option []
    }
    
    type GraphAccessFailure = 
        | InvalidVertexId of VertexId

    type GraphFailure =
        | GraphAccessFailure of GraphAccessFailure
        | FileAccessFailure of Exception
        | ParsingFailure of string
        | VisualisationFailure of Exception

    /// Main functional style error handling result type used throughout the Graphs namespace
    type GraphResult<'TSuccess> = Result<'TSuccess, GraphFailure>  


    type internal TwoColouring = Red | Green | Uncoloured
   
