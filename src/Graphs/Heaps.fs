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

    /// All ways that operations on a Heap ADT can fail    
    type [<StructuralEquality; NoComparison>] HeapFailure = 
        | EmptyHeap

    type HeapResult<'TSuccess> = Result<'TSuccess, HeapFailure>

    /// Whether the minimum key (item) value or the maximum key value is the highest priority in the heap        
    type [<StructuralEquality; NoComparison>] HeapRootOrdering = 
        | MinKey 
        | MaxKey

    /// How many children per heap tree node. These arities are all powers of two.
    /// The performance trade offs are similar to those between a binary search tree and a B+ Tree.
    /// The binary heap is a common implementation, but quaternary maybe better peformance in a wider range of applications.        
    type [<StructuralEquality; StructuralComparison>] HeapArity = 
        | Binary 
        | Quaternary 
        | Octonary 
        | Sendenary 
        | TerDenBinary

    /// The initial capacity of the heap        
    type [<StructuralEquality; NoComparison>] Capacity = 
        | Capacity of uint64 
        | DefaultCapacity

                    
        
    /// DHeap Abstract Data Type (ADT)        
    [<NoEquality; NoComparison>]
    [<StructuredFormatDisplay("DHeap Heap={Heap} Order={Order} Arity={TreeNodeChildCount}")>]
    type DHeap<'T when 'T : comparison> = private {
        Heap: ResizeArray<'T>              
        Order : HeapRootOrdering 
        IsHigherPriorityComparer: ('T -> 'T -> bool)
        TreeNodeChildCount: int
        TreeNodeChildCountAsPowerOfTwoExponent: int             
    }
                
    /// A mutable D-ary heap Abstact Data Type (ADT) supporting the basic operations:
    /// - insert, extractHighestPriority ("remove" "delete" "pop"), highestPriority ("peek") and extractHighestPriorityAndInsert all with O(n log n) complexity
    /// - size, isEmpty with O(1) complexity are pure functions
    [<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module DHeap = 

        /// Return the number of entries in the dheap priority queue
        let size (dheap: DHeap<'T>) : int = 
            dheap.Heap.Count

        /// Is the dheap priority queue empty
        let isEmpty (dheap: DHeap<'T>) : bool = 
            dheap.Heap.Count = 0


        /// Convert a power of two number into its value as a power of two integer exponent
        let private asPowerOfTwoExponent (n: int) : int = 
            Math.Log(float n, 2.0) |> int
    
        /// Extract the capacity value or return a default.
        let private capacityValue (capacity: Capacity) : int = 
            match capacity with
            | Capacity(cap) -> int cap 
            | DefaultCapacity -> 16

        /// Return the arity of a tree node in the heap as an integer
        let private arityToInteger (arity: HeapArity) : int = 
            match arity with
            | Binary -> 2
            | Quaternary -> 4
            | Octonary -> 8
            | Sendenary -> 16
            | TerDenBinary -> 32                       
       
        /// For a key identified by its index `n` in the heap, return the index of that key's kth child.
        let private indexOfKthChild (dheap: DHeap<'T>) (n: int) (k: int) : int =         
            (n <<< dheap.TreeNodeChildCountAsPowerOfTwoExponent) + k

        /// For a key identified by its index `n` in the heap, return the index of its parent.
        let private indexOfParent (dheap: DHeap<'T>) (n: int) : int =              
            (n - 1) >>> dheap.TreeNodeChildCountAsPowerOfTwoExponent // note this will not work if passed index 0, the root index of the heap tree (which is guarded against)

        /// Exchange the positions of two elements specified by index in the heap's array 
        let private swap (dheap: DHeap<'T>) (index1: int) (index2: int) = 
            let tmp = dheap.Heap.[index1]
            dheap.Heap.[index1] <- dheap.Heap.[index2]
            dheap.Heap.[index2] <- tmp

        /// Is the element at index1 higher priority than the element at index2
        let private isHigherPriority (dheap: DHeap<'T>) (index1: int) (index2: int) : bool = 
            let elem1 = dheap.Heap.[index1]
            let elem2 = dheap.Heap.[index2]
            dheap.IsHigherPriorityComparer elem1 elem2

        /// Return highest priority child index of a parent 'index'
        let private highestPriorityChildIndex (dheap: DHeap<'T>) (index: int) : int =
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
        let rec private sink (dheap: DHeap<'T>) (index: int) = 
            let highestChildIndex = highestPriorityChildIndex dheap index
            if highestChildIndex < (size dheap) && 
                isHigherPriority dheap highestChildIndex index then
                swap dheap highestChildIndex index
                sink dheap highestChildIndex

        /// Ensure that a key (identified by index) is lower priority than its parent and if not 
        /// swim it up the tree swapping with the parent (and grandparent etc) as required
        let rec private swim (dheap: DHeap<'T>) (index:int) =       
            if index <> 0 then
                let parentIndex = indexOfParent dheap index        
                if isHigherPriority dheap index parentIndex then
                    swap dheap index parentIndex   
                    swim dheap parentIndex


        /// Insert a key into the dheap priority queue    
        let insert (dheap: DHeap<'T>) (key: 'T) = 
            // Add to heap array, making it the last element and lowest priority and 
            // then swim it up to its proper place.
            dheap.Heap.Add(key)
            swim dheap <| (size dheap) - 1

        /// Inserts everything in the items seq into an *empty* dheap.
        let private heapifyItems (dheap: DHeap<'T>) (items: 'T seq) = 
            // We would use a simpler ```Seq.iter (fun item -> insert dheap item) items```
            // if we want a convenience bulk insert on a non-empty heap.
            if not(isEmpty dheap) then
                failwith "Programming logic error - heapify should only be used on an empty heap."
            dheap.Heap.AddRange(items)
            for i = dheap.Heap.Count/2 to 1 do
                sink dheap i

        /// Return a new DHeap optionally with the given items added
        let private makeDheap (arity: HeapArity) (minMaxType: HeapRootOrdering) (capacity: Capacity) (items : 'T seq option) : DHeap<'T> = 
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
                     
        /// Return a new empty DHeap.
        let empty (arity: HeapArity) (minMaxType: HeapRootOrdering) (capacity: Capacity) : DHeap<'T> = 
            makeDheap arity minMaxType capacity None
           
        /// Return the highest priority key from the dheap priority queue.
        /// This is the min or max key depending upon whether it is a min/max heap respectively.
        let highestPriority (dheap: DHeap<'T>) : HeapResult<'T> = 
            if not(isEmpty dheap) then 
                ok dheap.Heap.[0] 
            else
                fail EmptyHeap

        /// Return and remove the highest priority key from the dheap priority queue
        let extractHighestPriority (dheap: DHeap<'T>) : HeapResult<'T> =         
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

        /// Insert a key into and extract the highest priority key from the dheap priority queue
        /// Purely a performance optimisation that reduces the heap maintenance cost over separately
        /// calling `extractHighestPriority` then `insert`.
        let extractHighestPriorityAndInsert (dheap: DHeap<'T>) (newKey: 'T) : HeapResult<'T> = 
            if not(isEmpty dheap) then
                let highest = dheap.Heap.[0]
                dheap.Heap.[0] <- newKey
                sink dheap 0
                ok highest
            else
                fail EmptyHeap
    
                
    let minBinaryHeap : (unit -> DHeap<'T>) = fun _ -> DHeap.empty Binary MinKey DefaultCapacity 
    let maxBinaryHeap : (unit -> DHeap<'T>) = fun _ -> DHeap.empty Binary MaxKey DefaultCapacity
    let minQuaternaryHeap : (unit -> DHeap<'T>) = fun _ -> DHeap.empty Quaternary MinKey DefaultCapacity 
    let maxQuaternaryHeap : (unit -> DHeap<'T>) = fun _ -> DHeap.empty Quaternary MaxKey DefaultCapacity 

    let minBinaryHeapWith (items: seq<'T>) : DHeap<'T> = DHeap.ofSeq Binary MinKey DefaultCapacity items
    let maxBinaryHeapWith (items: seq<'T>) : DHeap<'T> = DHeap.ofSeq Binary MaxKey DefaultCapacity items
    let minQuaternaryHeapWith (items: seq<'T>) : DHeap<'T> = DHeap.ofSeq Quaternary MinKey DefaultCapacity items
    let maxQuaternaryHeapWith (items: seq<'T>) : DHeap<'T> = DHeap.ofSeq Quaternary MaxKey DefaultCapacity items






// Optional extra functionality to consider:
// 1) *custom comparators instead of just the natural IComparable ordering.
// 2) changePriority dheap key (re-adjust in the heap after the key is mutated externally). This is quite expensive in terms of overheads.        
//    See also https://github.com/contain-rs/discuss/issues/11 for impl with "stable handles".
// 3) deleteKey dheap key. Again quite expensive as you need a way to find the item in the heap quickly.        
// 4) merge dheap otherHeap - O(n) complexity for a dheap, but fast for something like a pairing heap.
// 5) meld dheap otherHeap - O(n) destroy both original heaps whilst creating a new one
    