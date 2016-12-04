module Graph.HeapTests

open Chessie.ErrorHandling
open FsCheck
open FsCheck.GenBuilder
open Fuchu
open FsUnit

open Graphs


module HeapTestUtils =
    let genHeap : Gen<Heaps.DHeap<int>> = 
        gen {        
            let! arity = Arb.generate<Heaps.HeapArity>                        
            let! order = Arb.generate<Heaps.HeapRootOrdering>
            let! cap = Arb.generate<Heaps.Capacity>
            let! contents = Gen.listOf Arb.generate<int>
            if List.length contents > 0 then
                return Heaps.DHeap.ofSeq arity order cap (Seq.ofList contents)
            else
                return Heaps.DHeap.empty arity order cap
        }
    type HeapGenerators = 
        static member DHeap() =
            { new Arbitrary<Heaps.DHeap<int>>() with
                  override this.Generator = genHeap
                  override this.Shrinker t = Seq.empty }

    type InsertExtractAction = 
        | Insertion of int
        | Removal
        | Replacement of int

    type FetchAction = 
        | FetchExtract
        | FetchReplacement of int 
        | FetchPeek
            
    let areElementsPriorityOrdered (order: Heaps.HeapRootOrdering) elements : bool = 
        let comparer = 
            match order with 
            | Heaps.MinKey -> fun (a, b) -> a <= b
            | Heaps.MaxKey -> fun (a, b) -> a >= b
        elements
        |> Seq.pairwise
        |> Seq.forall comparer
    
    let emptyHeapAndCheckIsPriorityOrdered (heap: Heaps.DHeap<int>) : bool = 
        let order = Heaps.DHeap.order heap
        let elemsCount = Heaps.DHeap.size heap                
        let removeElementOrFailTest = 
            fun _ -> Heaps.DHeap.extractHighestPriority heap 
                     |> returnOrFail        
        let elems = 
            [1..elemsCount] 
            |> List.map removeElementOrFailTest                 
        areElementsPriorityOrdered order elems

    let applyHeapActions (heap: Heaps.DHeap<int>) (heapInsertExtractActions: InsertExtractAction list) =
        let updateHeap (action: InsertExtractAction) = 
            match action with
            | Insertion(value) -> Heaps.DHeap.insert heap value
            | Removal -> Heaps.DHeap.extractHighestPriority heap |> ignore
            | Replacement(value) -> Heaps.DHeap.extractHighestPriorityAndInsert heap value |> ignore
        List.iter updateHeap heapInsertExtractActions

    

open HeapTestUtils    

[<Tests>]
let heapTests = 
    
    Arb.register<HeapGenerators>() |> ignore

    testList "Heap ADT" [              
        
        testProperty "A heap has a non-negative size" <|
            fun (heap: Heaps.DHeap<int>) ->
                Heaps.DHeap.size heap >= 0

        testProperty "A Heap isEmpty only when size is zero" <|
            fun (heap: Heaps.DHeap<int>) ->
                let size = Heaps.DHeap.size heap
                let empty = Heaps.DHeap.isEmpty heap
                (size = 0 && empty) || (not empty)

        testProperty "Can extractHighestPriority size times from non-empty heap until empty" <|
            fun (heap: Heaps.DHeap<int>) ->
                let size = Heaps.DHeap.size heap
                for  i = 1 to size do
                    let result = Heaps.DHeap.extractHighestPriority heap
                    returnOrFail result |> ignore
                Heaps.DHeap.isEmpty heap
                        
        testPropertyWithConfig {Config.Quick with StartSize = 0; EndSize = 0} "Singleton in, Singleton out" <|
            fun (heap: Heaps.DHeap<int>) (elem: int) ->
                Heaps.DHeap.insert heap elem
                let extracted: Heaps.HeapResult<int> = Heaps.DHeap.extractHighestPriority heap
                extracted = ok elem
            
        testProperty "N Insert/Remove call pairs result in the same initial size" <|
            fun (heap: Heaps.DHeap<int>) (ns: int list) ->
                let initialSize = Heaps.DHeap.size heap
                let insertExtractCallPair elem = 
                    Heaps.DHeap.insert heap elem |> ignore
                    Heaps.DHeap.extractHighestPriority heap |> returnOrFail |> ignore
                List.iter insertExtractCallPair ns
                Heaps.DHeap.size heap = initialSize

        testProperty "N extractHighestPriorityAndInsert calls to a non-empty heap results in the same initial size" <|
            fun (heap: Heaps.DHeap<int>) (ns: int list) ->
                // The StartSize property of the Config does not guarantee a non-empty input
                // although EndSize does seems to guarantee a limited max size to inputs.
                if Heaps.DHeap.isEmpty heap then
                    Heaps.DHeap.insert heap 42 // arbitrary value
                let initialSize = Heaps.DHeap.size heap    
                let initialEmpty = Heaps.DHeap.isEmpty heap           
                List.iter (Heaps.DHeap.extractHighestPriorityAndInsert heap >> returnOrFail >> ignore) ns                
                Heaps.DHeap.size heap = initialSize && Heaps.DHeap.isEmpty heap = initialEmpty
                
        testProperty "Peeking the highest priority does not affect the size of the heap" <|
            fun (heap: Heaps.DHeap<int>) ->
                let initialSize = Heaps.DHeap.size heap
                let initialEmpty = Heaps.DHeap.isEmpty heap
                Heaps.DHeap.highestPriority heap |> ignore
                Heaps.DHeap.size heap = initialSize && Heaps.DHeap.isEmpty heap = initialEmpty
                
        testPropertyWithConfig {Config.Quick with MaxTest=1000; EndSize=512} "Elements inserted come out in order" <|
            fun (heap: Heaps.DHeap<int>) ->
                emptyHeapAndCheckIsPriorityOrdered heap

        testPropertyWithConfig {Config.Quick with MaxTest=1000; EndSize=1024} 
            "After creation and further randomly ordered inserts/extractions elements come out in order" <|
            fun (heap: Heaps.DHeap<int>) (heapInsertExtractActions: InsertExtractAction list) ->
                applyHeapActions heap heapInsertExtractActions
                emptyHeapAndCheckIsPriorityOrdered heap

        testPropertyWithConfig {Config.Quick with MaxTest=1000; EndSize=1} 
            "Fetch operations on a non-empty heap succeed and operations on an empty heap produce a heap failure" <|
            fun (heap: Heaps.DHeap<int>) (fetchAction: FetchAction) ->
                let initiallyEmpty = Heaps.DHeap.isEmpty heap
                let result = 
                    match fetchAction with
                    | FetchExtract -> Heaps.DHeap.extractHighestPriority heap 
                    | FetchReplacement(newKey) -> Heaps.DHeap.extractHighestPriorityAndInsert heap newKey
                    | FetchPeek -> Heaps.DHeap.highestPriority heap
                let isHeapFailure = failed result 
                (initiallyEmpty && isHeapFailure) || ((not initiallyEmpty) && (not isHeapFailure))           
    ]
