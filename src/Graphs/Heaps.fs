namespace Graphs

open System
open Chessie.ErrorHandling

/// A heap (priority queue) contains a set of items x, each with a key k(x) 
/// from a totally ordered universe, and associated information. There are no ties in keys.
/// A heap is like a dictionary but no access by key - we only retrieve the
/// highest priority (min or max) key item.
/// With a reference to an item in the heap, we can decrease its key priority in a the heap 
/// or just delete it from the heap.
[<RequireQualifiedAccess>]
module DHeap = 
    
    type HeapRootOrdering = MinKey | MaxKey

    type DHeap<'T when 'T : comparison> = {            
        Heap: ResizeArray<'T> 
        ChildCountAsPowerOfTwoExponent: int 
        Order : HeapRootOrdering 
        IsHigherPriorityComparer: ('T -> 'T -> bool)
    }

    type HeapFailure = 
        | NonPowerOfTwoChildCount of int
        | EmptyHeap

    type HeapResult<'TSuccess> = Result<'TSuccess, HeapFailure>

    /// Is an integer a power of two
    let internal isPowerOfTwo n = 
        n > 0 && (n &&& (n - 1) = 0)

    /// Convert a power of two number into its value as a power of two exponent
    let internal asPowerOfTwoExponent n = 
        Math.Log(float n, 2.0) |> int
    
    /// Extract the given initial capacity for the heap size or provide a default.
    let internal startCapacity (initialCapacity: uint64 option) = 
        match initialCapacity with
        | Some(cap) -> int cap 
        | None -> 16

    /// Return a DHeap from the inputs provided if possible else fail
    let internal makeDheap perNodeChildCount (initialCapacity: uint64 option) minMaxType : HeapResult<DHeap<'T>> = 
        if isPowerOfTwo perNodeChildCount then 
            ok { Heap = new ResizeArray<'T>(startCapacity initialCapacity)
                 ChildCountAsPowerOfTwoExponent = asPowerOfTwoExponent perNodeChildCount 
                 Order = minMaxType 
                 IsHigherPriorityComparer = if minMaxType = MinKey then 
                                                (fun a b -> a < b)  
                                            else
                                                (fun a b -> a > b)}            
        else
            fail (NonPowerOfTwoChildCount perNodeChildCount)

    /// For a key identified by its index `n` in the heap, return the index of that key's kth child.
    let internal indexOfKthChild dheap n k =         
        (n <<< dheap.ChildCountAsPowerOfTwoExponent) + k

    /// For a key identified by its index `n` in the heap, return the index of its parent.
    let internal indexOfParent dheap n =  
        // note this will not work if passed index 0, the root index of the heap tree
        (n - 1) >>> dheap.ChildCountAsPowerOfTwoExponent

    /// Exchange the positions of two elements specified by index in the heap's array 
    let swap dheap index1 index2 = 
        let tmp = dheap.Heap.[index1]
        dheap.Heap.[index1] <- dheap.Heap.[index2]
        dheap.Heap.[index2] <- tmp

    /// Ensure that a key (identified by index) is higher priority than its children and if not 
    /// sink it down the tree swapping with a child (and grandchildren etc) as required
    let internal sink dheap index = 
        failwith "unimplemented"

    /// Ensure that a key (identified by index) is lower priority than its parent and if not 
    /// swim it up the tree swapping with the parent (and grandparent etc) as required
    let internal swim (dheap: DHeap<'T>) index =         
        let mutable elemIndex = index
        let mutable parentIndex = indexOfParent dheap elemIndex        
        let element = dheap.Heap.[index]

        while elemIndex <> 0 && dheap.IsHigherPriorityComparer element dheap.Heap.[parentIndex] do 

            swap dheap elemIndex parentIndex   
            elemIndex <- parentIndex
            parentIndex <- indexOfParent dheap elemIndex

    

    /// Create an empty dheap priority queue where the minimum key has the highest priority and 
    /// with an optional initial capacity
    let minHeap perNodeChildCount (initialCapacity: uint64 option) : HeapResult<DHeap<'T>> =   
        makeDheap perNodeChildCount initialCapacity MinKey      

    /// Create an empty dheap priority queue where the maximum key has the highest priority and 
    /// with an optional initial capacity
    let maxHeap perNodeChildCount (initialCapacity: uint64 option) : HeapResult<DHeap<'T>> =
        makeDheap perNodeChildCount initialCapacity MaxKey

    /// Return the number of entries in the dheap priority queue
    let size dheap = 
        dheap.Heap.Count

    /// Is the dheap priority queue empty
    let isEmpty dheap = 
        dheap.Heap.Count = 0
                
    /// Insert a key into the dheap priority queue    
    let insert dheap (key: 'T) = 
        // Add to heap array, making it the last element and lowest priority and 
        // then swim it up to its proper place.
        dheap.Heap.Add(key)
        swim dheap <| dheap.Heap.Count - 1

    /// Return the highest priority key from the dheap priority queue.
    /// This is the min or max key depending upon whether it is a min/max heap respectively.
    let highestPriority dheap : HeapResult<'T> = 
        if not(isEmpty dheap) then 
            ok dheap.Heap.[0] 
        else
            fail EmptyHeap

    /// Return and remove the highest priority key frm the dheap priority queue
    let removeHighestPriority dheap : HeapResult<'T> = 
        // exchange highest with end of array, remove it, then sink the highest item
        if not(isEmpty dheap) then
            let highest = ok dheap.Heap.[0]
            let lastElementIndex = dheap.Heap.Count - 1
            swap dheap 0 lastElementIndex
            sink dheap 0
            // ... dheap.Heap.Remove the last
            ok highest
        else
            fail EmptyHeap

    // Optional extra functionality to consider:
    // 1) increasePriority dheap key (re-adjust in the heap after the key is mutated externally)
    // 2) deleteKey dheap key
    // 3) custom comparators instead of just the natural IComparable ordering
    // 4) merghe dheap otherHeap - O(n) complexity for a dheap, but fast for something like a pairing heap.