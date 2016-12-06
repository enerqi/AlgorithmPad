namespace Graphs

open Chessie.ErrorHandling
open Nessos.Streams

[<AutoOpen>]
module DisjointSetModule = 

    /// Disjoint Set Abstract Data Type (ADT)
    type [<NoEquality; NoComparison>] DisjointSet = private {
        Forest: Forest 
        Ranks: Ranks
    }
    /// Trees encoded into array indices where the value at index[i] is the parent tree node of index[i]
    and private Forest = ResizeArray<int>
    /// The rank of a tree at index[i] in the Forest where rank is like tree depth except that the path 
    /// compression algorithim makes trees shallower over time 
    and private Ranks = ResizeArray<int>

    /// Type safe wrapper of a disjoint set index. Valid values are between Zero and DisjointSetSize - 1.
    type [<Struct>] EntryId = 
        val Index : uint32
        new (index: uint32) = {Index = index}

    /// The set size specifies the current valid range of EntryId values to be 0 to DisjointSetSize - 1.
    type [<StructuralEquality; NoComparison>] DisjointSetSize = 
        | Size of uint32
        | DefaultSize

    /// All ways that operations on a DisjointSet ADT can fail    
    type [<StructuralEquality; NoComparison>] DisjointSetFailure = 
        | InvalidEntryId of EntryId

    type DisjointSetResult<'TSuccess> = Result<'TSuccess, DisjointSetFailure>


    /// A mutable disjoint set (also known as "union find") data structure. It tracks a set of elements
    /// partitioned into a number fo disjoint subsets. In this Disjoint Set implementation the elements
    /// are numbers (EntryIds) in the range 0 to size of set - 1. The user must maintain their own separate 
    /// symbol table or other way of mapping the numbers (EntryIds) to other types. Operations supported:
    /// - `find`, `union` and `inSameSubset` all have amortized roughly small constant overhead - O(alpha(n)) 
    ///   where alpha(n) is the inverse Ackerman function.
    /// - The `make` constructor and `increaseSize` are linear in proportion to the number of new elements being 
    ///   added to the disjoint set.
    /// - The `ofSeq` constructor is the same as `make` but also calls union seq size number of times.
    ///
    /// Example:
    /// let unionFindSet = DisjointSet.make (Size 20) // entry ids 0-19 are all singleton subsets
    /// DisjointSet.union unionFindSet (EntryId 4) (EntryId 5) // {4 5} are one subset now
    /// DisjointSet.find unionFindSet  (EntryId 4)  // (EntryId 5) root of the subset
    /// DisjointSet.find unionFindSet  (EntryId 5)  // (EntryId 5)
    /// DisjointSet.union unionFindSet (EntryId 50) // DisjointSetFailure InvalidEntryId
    /// DisjointSet.find unionFindSet  (EntryId 50) // DisjointSetFailure InvalidEntryId
    /// DisjointSet.increaseSize       (Size 100)   // entry ids 0-99 are all valid. 20-99 are new singleton subsets
    /// DisjointSet.find unionFindSet  (EntryId 50) // (EntryId 50)
    /// DisjointSet.inSameSubset unionFindSet  (EntryId 4) (EntryId 5) // true
    /// DisjointSet.inSameSubset unionFindSet  (EntryId 5) (EntryId 6) // false
    ///
    /// let ufSet2 = DisjointSet.ofSeq (Size 10) (Seq.ofList [(EntryId 1, EntryId 2); (EntryId 1, EntryId 3)])
    /// DisjointSet.find ufSet2  (EntryId 1)  // (EntryId 2) root of the subset
    /// DisjointSet.find ufSet2  (EntryId 2)  // (EntryId 2) root of the subset
    /// DisjointSet.find ufSet2  (EntryId 3)  // (EntryId 2) root of the subset
    /// DisjointSet.find ufSet2  (EntryId 9)  // (EntryId 9) singleton subset
    /// DisjointSet.union ufSet2 (EntryId 0) (EntryId 9)  // etc.
    [<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module DisjointSet = 

        /// Extract the size as an integer or return a default.
        let private sizeValue (size: DisjointSetSize) : int = 
            match size with
            | Size(s) -> int s 
            | DefaultSize -> 16

        /// Return the index of an EntryId in a format compatible with the ResizeArray index type.
        let private asIndex (entryId: EntryId) : int = 
            int entryId.Index

        /// Initialise new parts of the array backed forest so that new entries are trees with one node, 
        /// abstractly making them singleton disjoint sub sets. 
        /// The entry at index i in this array encoded forest of tree(s) points to itself initially as there 
        /// are no other nodes in that tree to start with and it is the root of its own tree.
        let private seedForest (forest: Forest) (newSize: int) : unit = 
            // If an entry already exists in the array then we assume that it is already seeded and in use.
            let forestPopulation = forest.Count
            let toSeedCount = newSize - forestPopulation
            for i = forestPopulation to (toSeedCount - 1) do 
                forest.Add(i)

        /// Initialise the rank information for new parts of the forest so we know that the new singleton subsets
        /// have 1 object in them. 
        /// The entry at index i in the rank array says how many objects are part of the same subset (have the same tree root).
        let private seedRanks (ranks: Ranks) (newSize: int) : unit =
            // If an entry already exists in the array then we assume that it is already seeded and in use.
            let ranksSize = ranks.Count
            let toSeedCount = newSize - ranksSize
            for i = ranksSize to (toSeedCount - 1) do 
                ranks.Add(1)

        /// Return the subset id for entry `p` within the given Forest array. 
        /// Apply path compression when looking up the subtree's root so it is an O(1) lookup next time.
        let rec private findWithinForest (forest: Forest) (p: EntryId) : EntryId = 
            // If p is the root of the tree its parent is itself
            // If p is not the root of its own tree then make sure it references
            // the root of the tree and return that value as the subset id (subset representative)
            let entryIndex = asIndex p
            let parentIndex = forest.[entryIndex]
            let isTreeRoot = parentIndex = entryIndex

            if not isTreeRoot then 
                // find root of subset, which maybe a grandparent etc.
                let treeRootEntryId = findWithinForest forest (EntryId (uint32 parentIndex))
                let treeRootIndex = asIndex treeRootEntryId

                // do path compression so that it is quicker to find the set root next time
                if parentIndex <> treeRootIndex then
                    forest.[entryIndex] <- treeRootIndex

                treeRootEntryId
            else 
                p

        /// Return the size of the range of EntryIds (0 to size-1) supported by the disjoint set. 
        /// For example, a size of 10 means EntryId 0 up to EntryId 9 are the only valid Ids.
        let size (disjointSet: DisjointSet) : int = 
            disjointSet.Forest.Count

        /// Union the two subsets of `p` and `q`
        let union (disjointSet: DisjointSet) (p: EntryId) (q: EntryId) : DisjointSetResult<unit> = 
            let forest = disjointSet.Forest
            let forestSize = forest.Count 
            let pIsValid = (asIndex p) < forestSize
            let qIsValid = (asIndex q) < forestSize

            if pIsValid && qIsValid then

                let pRoot = findWithinForest forest p
                let qRoot = findWithinForest forest q

                if pRoot <> qRoot then 
                    // Different trees (subsets) so
                    // Attach the smaller tree to the root of the larger tree and update the ranks
                    let ranks = disjointSet.Ranks                    
                    let pRootIndex = asIndex pRoot
                    let qRootIndex = asIndex qRoot
                    let pRootRank = ranks.[pRootIndex]
                    let qRootRank = ranks.[qRootIndex]

                    if pRootRank < qRootRank then 
                        forest.[pRootIndex] <- qRootIndex // point p the smaller ranked tree at the larger tree q 
                        ranks.[qRootIndex] <- qRootRank + pRootRank // q then grows by the rank of p
                    else
                        forest.[qRootIndex] <- pRootIndex
                        ranks.[pRootIndex] <- pRootRank + qRootRank

                ok ()
            
            else
                fail (InvalidEntryId (if not pIsValid then p else q))

        /// Return the subset id for entryId `p`.
        let find (disjointSet: DisjointSet) (p: EntryId) : DisjointSetResult<EntryId> = 
            let forestSize = disjointSet.Forest.Count
            if (asIndex p) < forestSize then
                ok (findWithinForest disjointSet.Forest p)
            else
                fail (InvalidEntryId p)

        /// Are `p` and `q` in the same subset?
        let inSameSubset (disjointSet: DisjointSet) (p: EntryId) (q: EntryId) : DisjointSetResult<bool> = 
            trial {
                let! pSubSet = find disjointSet p
                let! qSubSet = find disjointSet q
                return pSubSet = qSubSet
            }

        /// Return a new DisjointSet with the given capacity that has all the unifications operations between the given
        /// entry ids applied.
        let private makeDisjointSet (size: DisjointSetSize) (unifications : option<seq<(EntryId * EntryId)>>) : DisjointSet = 
            let initialSize = sizeValue size                
            
            let makeForest (forestSize: int) : Forest = 
                let forest = new Forest(forestSize)
                seedForest forest initialSize    
                forest            

            let sizeWithUnions (unions: seq<(EntryId * EntryId)>) : int * (EntryId * EntryId) array option = 
                let unificationPairs = Array.ofSeq unions
                let calcMaxPairValue = fun (a, b) -> max a b
                let maxEntrySize =         
                    unificationPairs            
                    |> Stream.ofArray
                    |> Stream.maxBy calcMaxPairValue
                    |> calcMaxPairValue
                let sizeRequired = max initialSize (asIndex maxEntrySize)
                (sizeRequired, Some unificationPairs)
                                            
            let (forestSize, unions) = 
                match unifications with 
                | Some(unions) -> sizeWithUnions unions        
                | None -> (initialSize, None)

            let forest = makeForest forestSize
            let ranks = new Ranks(forestSize)
            seedRanks ranks forestSize

            let disjointSet = 
                { Forest = forest
                  Ranks = ranks }

            match unions with
            | Some(unionPairs) -> unionPairs |> Array.iter (fun (entry1, entry2) -> union disjointSet entry1 entry2 |> returnOrFail)
            | None -> ()

            disjointSet

        /// Return a new DisjointSet supporting the specified size. Each entry is initially a singleton sub set
        /// meaning they are unioned with nothing.
        let make (size: DisjointSetSize) : DisjointSet =        
            makeDisjointSet size None      
        
        /// Return a new DisjointSet supporting the the specified size or a larger size if the highest EntryId 
        /// in `unifications` requires it.         
        /// The sequence of unifications is applied to the DisjointSet (foreach: union entryId1 entryId2).
        let ofSeq (size: DisjointSetSize) (unifications : seq<(EntryId * EntryId)>) : DisjointSet = 
            makeDisjointSet size (Some unifications)

        /// Ensure the disjoint set supports the specified size. If new entry Ids are added they are all 
        /// created as singleton sub sets.
        let increaseSize (disjointSet: DisjointSet) (size: DisjointSetSize) : unit = 
            seedForest disjointSet.Forest (sizeValue size)
            seedRanks disjointSet.Ranks (sizeValue size)


type DisjointSet = DisjointSetModule.DisjointSet

