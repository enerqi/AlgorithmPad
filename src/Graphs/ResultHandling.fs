namespace Graphs

open Chessie.ErrorHandling

[<AutoOpen>]
module ResultHandling = 

    let internal convertResultMessageType (f: 'TMessage -> 'TOtherMessage) (result: Result<'TSuccess, 'TMessage>) 
        : Result<'TSuccess, 'TOtherMessage> = 

        match result with 
        | Ok(v, msgs) -> Ok(v, List.map f msgs)
        | Bad(msgs) -> Bad(List.map f msgs)

    /// Convert a HeapResult into the more general GraphResult type.
    let heapToGraphResult (heapResult: Heaps.HeapResult<'TSuccess>) : GraphResult<'TSuccess> = 
    
        let heapToGraphFailure (heapFailure: Heaps.HeapFailure) : GraphFailure =
            AlgorithmFailure (PriorityQueueFailure heapFailure)
        
        convertResultMessageType heapToGraphFailure heapResult
                
    /// Convert a DisjointSetResult into the more general GraphResult type.
    let disjointToGraphResult (disjointSetResult: DisjointSetResult<'TSuccess>) : GraphResult<'TSuccess> = 

        let disjointSetToGraphFailure (setFailure: DisjointSetFailure) : GraphFailure = 
            AlgorithmFailure (UnionFindFailure setFailure)

        convertResultMessageType disjointSetToGraphFailure disjointSetResult
