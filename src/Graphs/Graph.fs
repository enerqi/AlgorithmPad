namespace Graphs

module Graph = 

    let vertexFromId graph (v: VertexId) =
        if v.Id > 0 then 
            graph.Vertices.[v.Id]
        else 
            failwith "<= zero is an invalid vertex id"

    let verticesSeq graph = 
        Seq.ofArray graph.Vertices |> Seq.skip 1

