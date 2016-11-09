namespace Graphs

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Graph = 

    let vertexFromId graph (v: VertexId) : GraphResult<Vertex> =
        if v.Id > 0 && v.Id < graph.Vertices.Length then 
            Ok graph.Vertices.[v.Id]
        else 
            Error <| sprintf "Invalid vertex Id: %A" v
            
    let verticesSeq graph : seq<Vertex> = 
        Seq.ofArray graph.Vertices |> Seq.skip 1

