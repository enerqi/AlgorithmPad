namespace Graphs

[<AutoOpen>]
module DomainTypes = 

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

