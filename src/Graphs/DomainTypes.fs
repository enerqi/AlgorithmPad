namespace Graphs

[<AutoOpen>]
module DomainTypes = 

    type ErrorString = string
    type GraphResult<'TSuccess> = Result<'TSuccess, ErrorString>

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

    type Distance = 
        struct 
            val Distance: uint32
            new(d: uint32) = {Distance = d}
        end

    type BFS = {
        Source: VertexId
        ShortestPathDistances: Distance option []
        ShortestPathTree: VertexId option []
    }

    type internal TwoColouring = Red | Green | Uncoloured
   
