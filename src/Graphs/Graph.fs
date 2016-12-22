namespace Graphs

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Graph = 

    open Chessie.ErrorHandling

    let internal vertexFromArray (vertArray: Vertex array) (v: VertexId) : GraphResult<Vertex> = 
        tryF (fun _ -> vertArray.[v.Id]) 
             (fun _ -> GraphAccessFailure (InvalidVertexId v))


    /// Access a vertex in the graph via a VertexId.
    let vertexFromId (graph: Graph) (v: VertexId) : GraphResult<Vertex> =
        vertexFromArray graph.Vertices v
                    
    /// Return sequence of all the vertices in the graph.
    let verticesSeq (graph: Graph) : seq<Vertex> = 
        // Skip may throw in theory but we always have at least 1 item - the ignored index 0
        // that is only there to make indexing simpler when using 1 based indices for the 
        // serialised graph file formats
        Seq.ofArray graph.Vertices |> Seq.skip 1

    