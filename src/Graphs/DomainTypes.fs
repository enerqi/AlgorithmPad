namespace Graphs

open System.Diagnostics

[<AutoOpen>]
module DomainTypes = 
   
    open Chessie.ErrorHandling
    open System
    open System.Collections.Generic

    /// Main Graph datastructure type implemented with mutable Vertex structures
    [<StructuredFormatDisplay("Graph Directed={IsDirected} Weighted={IsWeighted} Vertices={VerticesCount} Edges={EdgesCount}")>]
    [<DebuggerDisplay("Graph Directed={IsDirected} Weighted={IsWeighted} Vertices={VerticesCount} Edges={EdgesCount}")>]
    type Graph = {
        Vertices: Vertex array 
        Weights: Dictionary<Source * Destination, Weight>
        IsDirected: bool
        IsWeighted: bool
        VerticesCount: int
        EdgesCount: int
    }
    /// Vertex (Node) in a graph using an adjacency list representation
    and [<StructuredFormatDisplay("Vertex {Identifier.Id}")>] [<DebuggerDisplay("Vertex {Identifier.Id}")>]
    Vertex = {    
        Identifier: VertexId
        Neighbours: ResizeArray<VertexId>
    } 
    /// Type safe wrapper of vertex integer id    
    and [<StructuredFormatDisplay("VertexId {Id}")>] [<DebuggerDisplay("VertexId {Id}")>]
    VertexId = 
        struct
            val Id: int
            new(id: int) = {Id = id}
        end       
    and [<StructuredFormatDisplay("Weight {Value}")>] [<DebuggerDisplay("Weight {Value}")>]
    Weight = 
        struct 
            val Value: int
            new(weight: int) = {Value = weight}
        end    
    and [<StructuredFormatDisplay("{DebuggerDisplayProperty}")>] [<DebuggerDisplay("{DebuggerDisplayProperty}")>] 
    Edge = 
        struct
            val Source: Source
            val Destination: Destination
            val Weight: Weight option
            new(src: Source, dst: Destination) = 
                {Source = src; Destination = dst; Weight = None}
            new(src: Source, dst: Destination, w: Weight) = 
                {Source = src; Destination = dst; Weight = Some w}
            
            member private this.DebuggerDisplayProperty = 
                match this.Weight with 
                | Some(w) -> sprintf "Source=%d Destination=%d Weight=%d" this.Source.VId.Id this.Destination.VId.Id w.Value
                | None -> sprintf "Source=%d Destination=%d Weight=null" this.Source.VId.Id this.Destination.VId.Id         
        end
    and [<StructuredFormatDisplay("Source {VId.Id}")>] [<DebuggerDisplay("Source {VId.Id}")>]
    Source = 
        struct 
            val VId: VertexId
            new(src: VertexId) = {VId = src}
        end
    and [<StructuredFormatDisplay("Destination {VId.Id}")>] [<DebuggerDisplay("Destination {VId.Id}")>]
    Destination = 
        struct 
            val VId: VertexId
            new(dst: VertexId) = {VId = dst}
        end
             

    /// Breadth first search results from a source vertex
    [<StructuredFormatDisplay("ShortestPaths Source={Source.Id}")>] 
    [<DebuggerDisplay("ShortestPaths Source={Source.Id}")>]
    type ShortestPaths = {
        Source: VertexId
        ShortestPathDistances: Distance option []
        ShortestPathTree: VertexId option []
    }
    /// Type safe wrapper of distance integer values
    and [<StructuredFormatDisplay("Distance {Distance}")>] [<DebuggerDisplay("Distance {Distance}")>]
    Distance = 
        struct 
            val Distance: uint32
            new(d: uint32) = {Distance = d}
        end    

    [<StructuredFormatDisplay("PathKey Distance={Distance} VertexId={Id}")>] 
    [<DebuggerDisplay("PathKey Distance={Distance} VertexId={Id}")>]
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
        | GraphInvalidTypeFailure of GraphInvalidTypeFailure
        | FileAccessFailure of Exception
        | ParsingFailure of string
        | VisualisationFailure of Exception
        | AlgorithmFailure of AlgorithmFailure
    and GraphAccessFailure = 
        | InvalidVertexId of VertexId
    and GraphInvalidTypeFailure = 
        | Directed
        | Undirected
        | Weighted
        | Unweighted
    and AlgorithmFailure =
        | PriorityQueueFailure of Heaps.HeapFailure
        | UnionFindFailure of DisjointSetFailure

    /// Main functional style error handling result type used throughout the Graphs namespace
    type GraphResult<'TSuccess> = Result<'TSuccess, GraphFailure>  


    type internal TwoColouring = Red | Green | Uncoloured
   
