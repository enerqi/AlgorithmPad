namespace Graphs

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Graph = 

    let vertexFromId graph (v: VertexId) : GraphResult<Vertex> =
        if v.Id > 0 && v.Id < graph.Vertices.Length then 
            Ok graph.Vertices.[v.Id]
        else 
            Error <| sprintf "Invalid vertex Id: %A" v
            
    let verticesSeq graph : seq<Vertex> = 
        // Skip may throw in theory but we always have at least 1 item - the ignored index 0
        // that is only there to make indexing simpler when using 1 based indices for the 
        // serialised graph file formats
        Seq.ofArray graph.Vertices |> Seq.skip 1

