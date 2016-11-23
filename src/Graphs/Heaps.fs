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
        TreeNodeChildCountAsPowerOfTwoExponent: int 
        TreeNodeChildCount: int 
        Order : HeapRootOrdering 
        IsHigherPriorityComparer: ('T -> 'T -> bool)
    }

    type HeapFailure = 
        | NonPowerOfTwoChildCount of int
        | EmptyHeap

    type HeapResult<'TSuccess> = Result<'TSuccess, HeapFailure>

    /// Return the number of entries in the dheap priority queue
    let size dheap = 
        dheap.Heap.Count

     /// Is the dheap priority queue empty
    let isEmpty dheap = 
        dheap.Heap.Count = 0


    /// Is an integer a positive power of two
    let internal isPositivePowerOfTwo n = 
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
        if isPositivePowerOfTwo perNodeChildCount then 
            ok { Heap = new ResizeArray<'T>(startCapacity initialCapacity)
                 TreeNodeChildCountAsPowerOfTwoExponent = asPowerOfTwoExponent perNodeChildCount 
                 TreeNodeChildCount = perNodeChildCount
                 Order = minMaxType 
                 IsHigherPriorityComparer = if minMaxType = MinKey then 
                                                (fun a b -> a < b)  
                                            else
                                                (fun a b -> a > b)}            
        else
            fail (NonPowerOfTwoChildCount perNodeChildCount)

    /// For a key identified by its index `n` in the heap, return the index of that key's kth child.
    let internal indexOfKthChild dheap n k =         
        (n <<< dheap.TreeNodeChildCountAsPowerOfTwoExponent) + k

    /// For a key identified by its index `n` in the heap, return the index of its parent.
    let internal indexOfParent dheap n =  
        // note this will not work if passed index 0, the root index of the heap tree, which is guarded against
        (n - 1) >>> dheap.TreeNodeChildCountAsPowerOfTwoExponent

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

        // TODO ? Could be a funky fold...
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
    let rec internal swim (dheap: DHeap<'T>) index =       
        if index <> 0 then
            let parentIndex = indexOfParent dheap index        
            if isHigherPriority dheap index parentIndex then
                swap dheap index parentIndex   
                swim dheap parentIndex

    
    /// Create an empty dheap priority queue where the minimum key has the highest priority and 
    /// with an optional initial capacity
    let minHeap perNodeChildCount (initialCapacity: uint64 option) : HeapResult<DHeap<'T>> =   
        makeDheap perNodeChildCount initialCapacity MinKey      

    /// Create an empty dheap priority queue where the maximum key has the highest priority and 
    /// with an optional initial capacity
    let maxHeap perNodeChildCount (initialCapacity: uint64 option) : HeapResult<DHeap<'T>> =
        makeDheap perNodeChildCount initialCapacity MaxKey           
                   
    /// Insert a key into the dheap priority queue    
    let insert dheap (key: 'T) = 
        // Add to heap array, making it the last element and lowest priority and 
        // then swim it up to its proper place.
        dheap.Heap.Add(key)
        swim dheap <| (size dheap) - 1

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

    // Optional extra functionality to consider:
    // 1) increasePriority dheap key (re-adjust in the heap after the key is mutated externally)
    //      debatable whether the overhead of updating a parallel hashmap is worth it, in some algorithms like djikstras
    //      duplicates may not be much of an issue.
    //      find (via hashtable), remove, mutate/change-comparator, reinsert
    //      See also https://github.com/contain-rs/discuss/issues/11 for impl with "stable handles".
    //      * A fibonacci heap is O(1) at changingPriority but quite a bit slower in general
    // 2) deleteKey dheap key
    //      again, find via hashtable maintenance otherwise O(n)    
    // 3) custom comparators instead of just the natural IComparable ordering
    // 4) merge dheap otherHeap - O(n) complexity for a dheap, but fast for something like a pairing heap.
    // 4b) meld dheap otherHeap - destroy both original heaps whilst creating a new one
    // 5) replace - pop root and push new key. Balances once as done at the same time.
    // 6) heapify - construct inplace on a given array of elements or copy a source seq: 
    //        for i=[A.length/2] downto 1: sink(i)