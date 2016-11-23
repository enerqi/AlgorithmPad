namespace Graphs

open System
open Chessie.ErrorHandling

/// A heap is one concrete implementation of a priority queue.
/// A priority queue is an abstract data type which is like a regular queue or stack data structure, 
/// but where additionally each element has a "priority" associated with it. In a priority queue, an 
/// element with high priority is served before an element with low priority. If two elements have the 
/// same priority, they are served according to their order in the queue.
/// A heap is a tree based datastructure satifying the heap property - see https://en.wikipedia.org/wiki/Heap_(data_structure)
/// This heap can act as a min or max heap, so we use the terminology "highest priority" to refer to the 
/// what is the max item in a max heap or min item in a min heap.
module Heaps =

    /// A mutable D-ary heap Abstact Data Type (ADT) supporting the basic operations:
    /// - empty O(1).
    /// - insert O(n log n).
    /// - extractHighestPriority O(n log n). ("Remove" "delete" "pop").
    /// - highestPriority O(n log n). ("Peek").
    /// - size [pure function] O(1).
    /// - isEmpty [pure function] O(1).
    [<RequireQualifiedAccess>]
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module DHeap = 

        // Optional extra functionality to consider:
        // 1) changePriority dheap key (re-adjust in the heap after the key is mutated externally). This is quite expensive in terms of overheads.        
        //    See also https://github.com/contain-rs/discuss/issues/11 for impl with "stable handles".
        // 2) deleteKey dheap key. Again quite expensive as you need a way to find the item in the heap quickly.        
        // 3) *custom comparators instead of just the natural IComparable ordering.
        // 4) merge dheap otherHeap - O(n) complexity for a dheap, but fast for something like a pairing heap.
        // 5) meld dheap otherHeap - destroy both original heaps whilst creating a new one
        // 5) replace - pop root and push new key. Balances the tree once as done at the same time.
        // 6) *heapify - construct inplace on a given array of elements or copy a source seq: for i=[A.length/2] downto 1: sink(i)
        //    Mutating an existing array instead of using a convenient ResizeArray -> Would have to "own" and handle array resize and keep explicit item counts.
        //    Copying an input sequence is straightforward (as long as items are not mutated externally whilst in the heap in a way that would affect the key comparison).
    
        /// Whether the minimum key (item) value or the maximum key value is the highest priority in the heap
        type HeapRootOrdering = 
            | MinKey 
            | MaxKey

        /// How many children per heap tree node. These arities are all powers of two.
        /// The performance trade offs are similar to those between a binary search tree and a B+ Tree.
        /// The binary heap is a common implementation, but quaternary maybe better peformance in a wider range of applications.
        type HeapArity = 
            | Binary 
            | Quaternary 
            | Octonary 
            | Sendenary 
            | TerDenBinary

        /// The initial capacity of the heap
        type Capacity = 
            | Capacity of uint64 
            | DefaultCapacity

        /// DHeap Abstract Data Type (ADT)
        type DHeap<'T when 'T : comparison> = private { // private record contents, not "type private DHeap" so we have an ADT. 
            Heap: ResizeArray<'T>              
            Order : HeapRootOrdering 
            IsHigherPriorityComparer: ('T -> 'T -> bool)
            TreeNodeChildCount: int
            TreeNodeChildCountAsPowerOfTwoExponent: int             
        }

        /// All ways that operations on the DHeap ADT can fail
        type HeapFailure = 
            | EmptyHeap

        type HeapResult<'TSuccess> = Result<'TSuccess, HeapFailure>

        /// Return the number of entries in the dheap priority queue
        let size dheap = 
            dheap.Heap.Count

         /// Is the dheap priority queue empty
        let isEmpty dheap = 
            dheap.Heap.Count = 0


        /// Convert a power of two number into its value as a power of two integer exponent
        let internal asPowerOfTwoExponent n = 
            Math.Log(float n, 2.0) |> int
    
        /// Extract the capacity value or return a default.
        let internal capacityValue capacity = 
            match capacity with
            | Capacity(cap) -> int cap 
            | DefaultCapacity -> 16

        /// Return the arity of a tree node in the heap as an integer
        let internal arityToInteger arity = 
            match arity with
            | Binary -> 2
            | Quaternary -> 4
            | Octonary -> 8
            | Sendenary -> 16
            | TerDenBinary -> 32                       
       
        /// For a key identified by its index `n` in the heap, return the index of that key's kth child.
        let internal indexOfKthChild dheap n k =         
            (n <<< dheap.TreeNodeChildCountAsPowerOfTwoExponent) + k

        /// For a key identified by its index `n` in the heap, return the index of its parent.
        let internal indexOfParent dheap n =              
            (n - 1) >>> dheap.TreeNodeChildCountAsPowerOfTwoExponent // note this will not work if passed index 0, the root index of the heap tree (which is guarded against)

        /// Exchange the positions of two elements specified by index in the heap's array 
        let internal swap dheap index1 index2 = 
            let tmp = dheap.Heap.[index1]
            dheap.Heap.[index1] <- dheap.Heap.[index2]
            dheap.Heap.[index2] <- tmp

        /// Is the element at index1 higher priority than the element at index2
        let internal isHigherPriority dheap index1 index2 = 
            let elem1 = dheap.Heap.[index1]
            let elem2 = dheap.Heap.[index2]
            dheap.IsHigherPriorityComparer elem1 elem2

        /// Return highest priority child index of a parent 'index'
        let internal highestPriorityChildIndex dheap index =
            let firstChildIndex = indexOfKthChild dheap index 1
            let lastChildIndex = indexOfKthChild dheap index dheap.TreeNodeChildCount
            let heapSize = (size dheap)
            let mutable highest = firstChildIndex

            for i = firstChildIndex + 1 to lastChildIndex do 
                if i < heapSize && isHigherPriority dheap i highest then 
                    highest <- i
            highest

        /// Ensure that a key (identified by index) is higher priority than its children and if not 
        /// sink it down the tree swapping with a child (and grandchildren etc) as required
        let rec internal sink dheap index = 
            let highestChildIndex = highestPriorityChildIndex dheap index
            if highestChildIndex < (size dheap) && 
               isHigherPriority dheap highestChildIndex index then
               swap dheap highestChildIndex index
               sink dheap highestChildIndex

        /// Ensure that a key (identified by index) is lower priority than its parent and if not 
        /// swim it up the tree swapping with the parent (and grandparent etc) as required
        let rec internal swim dheap index =       
            if index <> 0 then
                let parentIndex = indexOfParent dheap index        
                if isHigherPriority dheap index parentIndex then
                    swap dheap index parentIndex   
                    swim dheap parentIndex


        /// Insert a key into the dheap priority queue    
        let insert dheap (key: 'T) = 
            // Add to heap array, making it the last element and lowest priority and 
            // then swim it up to its proper place.
            dheap.Heap.Add(key)
            swim dheap <| (size dheap) - 1

        /// Inserts everything in the items seq into an *empty* dheap.
        let internal heapifyItems dheap items = 
            // We would use a simpler ```Seq.iter (fun item -> insert dheap item) items```
            // if we want a convenience bulk insert on a non-empty heap.
            if not(isEmpty dheap) then
                failwith "Programming logic error - heapify should only be used on an empty heap."
            dheap.Heap.AddRange(items)
            for i = dheap.Heap.Count/2 to 1 do
                sink dheap i

        /// Return a new DHeap optionally with the given items added
        let internal makeDheap (arity: HeapArity) (minMaxType: HeapRootOrdering) (capacity: Capacity) (items : 'T seq option) : DHeap<'T> = 
            let arityAsInt = arityToInteger arity
            let initialCapacity = capacityValue capacity
            let priorityComparer = if minMaxType = MinKey then 
                                       (fun a b -> a < b)  
                                   else
                                       (fun a b -> a > b)
            let heap = 
                { Heap = new ResizeArray<'T>(initialCapacity)
                  Order = minMaxType 
                  IsHigherPriorityComparer = priorityComparer
                  TreeNodeChildCount = arityAsInt
                  TreeNodeChildCountAsPowerOfTwoExponent = asPowerOfTwoExponent arityAsInt }                  
            
            match items with
            | Some(itemsSeq) -> heapifyItems heap itemsSeq
            | _ -> ()
            
            heap

        /// Return a new DHeap with the items in the given sequence added to it.
        let ofSeq (arity: HeapArity) (minMaxType: HeapRootOrdering) (capacity: Capacity) (items : 'T seq) : DHeap<'T> = 
            makeDheap arity minMaxType capacity (Some items)
                     
        // Return a new empty DHeap.
        let empty (arity: HeapArity) (minMaxType: HeapRootOrdering) (capacity: Capacity) : DHeap<'T> = 
            makeDheap arity minMaxType capacity None
           
        /// Return the highest priority key from the dheap priority queue.
        /// This is the min or max key depending upon whether it is a min/max heap respectively.
        let highestPriority dheap : HeapResult<'T> = 
            if not(isEmpty dheap) then 
                ok dheap.Heap.[0] 
            else
                fail EmptyHeap

        /// Return and remove the highest priority key from the dheap priority queue
        let extractHighestPriority dheap : HeapResult<'T> =         
            if not(isEmpty dheap) then
                // Swap highest with end of array, remove it, then sink the top item
                let highest = dheap.Heap.[0]
                let lastElementIndex = (size dheap) - 1
                swap dheap 0 lastElementIndex
                dheap.Heap.RemoveAt(lastElementIndex)
                sink dheap 0
                ok highest
            else
                fail EmptyHeap
                        

    let minBinaryHeap = fun _ -> DHeap.empty DHeap.Binary DHeap.MinKey DHeap.DefaultCapacity 
    let maxBinaryHeap = fun _ -> DHeap.empty DHeap.Binary DHeap.MaxKey DHeap.DefaultCapacity
    let minQuaternaryHeap = fun _ -> DHeap.empty DHeap.Quaternary DHeap.MinKey DHeap.DefaultCapacity 
    let maxQuaternaryHeap = fun _ -> DHeap.empty DHeap.Quaternary DHeap.MaxKey DHeap.DefaultCapacity 

    let minBinaryHeapWith items = DHeap.ofSeq DHeap.Binary DHeap.MinKey DHeap.DefaultCapacity items
    let maxBinaryHeapWith items = DHeap.ofSeq DHeap.Binary DHeap.MaxKey DHeap.DefaultCapacity items
    let minQuaternaryHeapWith items = DHeap.ofSeq DHeap.Quaternary DHeap.MinKey DHeap.DefaultCapacity items
    let maxQuaternaryHeapWith items = DHeap.ofSeq DHeap.Quaternary DHeap.MaxKey DHeap.DefaultCapacity items
