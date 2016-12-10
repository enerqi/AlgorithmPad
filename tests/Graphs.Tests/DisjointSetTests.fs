module Graph.DisjointSetTests

open Chessie.ErrorHandling
open FsCheck
open FsCheck.GenBuilder
open Fuchu
open FsUnit

open Graphs

module DisjointSetTestUtils =

    let genEntry : Gen<EntryId> = 
        gen {
            let! index = Arb.generate<uint32>
            return (EntryId index)
        }

    let genSet : Gen<DisjointSet> = 
        gen {
            let! size = Arb.generate<DisjointSetSize>

            let! unions = Gen.listOf Arb.generate<EntryId * EntryId>

            if List.length unions > 0 then 
                return DisjointSet.ofSeq size (Seq.ofList unions)
            else 
                return DisjointSet.make size
        }
            
    type DisjointSetGenerators = 
        static member DisjointSet() = 
            { new Arbitrary<DisjointSet>() with
                  override this.Generator = genSet 
                  override this.Shrinker t = Seq.empty }
        static member EntryId() = 
            { new Arbitrary<EntryId>() with 
                  override this.Generator = genEntry
                  override this.Shrinker t = Seq.empty }

    type Operation = 
        | Find
        | Union
          

    let subSetMappings (disjointSet: DisjointSet) : EntryId array = 
        let size = DisjointSet.size disjointSet
        [|0..size-1|] 
        |> Array.map (fun i -> 
            let entry = EntryId (uint32 i)
            DisjointSet.find disjointSet entry |> returnOrFail
        )

    let subSets (disjointSet: DisjointSet) : EntryId array =
        subSetMappings disjointSet        
        |> Array.distinct
        |> Array.sort

    let subSetsCount (disjointSet: DisjointSet) : int = 
        subSets disjointSet |> Array.length 

    let sizeOnePlusCondition (disjointSet: DisjointSet) : bool =
        DisjointSet.size disjointSet > 0

    let hasOnePlusSubSets (disjointSet: DisjointSet) : bool = 
        let setsCount = subSetsCount disjointSet
        let size = DisjointSet.size disjointSet
        setsCount >= 1 && setsCount <= size        

    let isEmpty (disjointSet: DisjointSet) : bool = 
        not (hasOnePlusSubSets disjointSet)

    let entryValid (e: EntryId) (maxIndex: int) : bool = 
        (int e.Index) <= maxIndex

    let randomEntry (maxIndex: int) : EntryId = 
        Gen.choose(0, maxIndex) 
        |> Gen.sample 0 1 
        |> List.head 
        |> uint32 
        |> EntryId
            

open DisjointSetTestUtils

[<Tests>]
let disjointSetTests = 
    Arb.register<DisjointSetGenerators>() |> ignore

    testList "DisjointSet ADT" [

        testPropertyWithConfig {Config.Quick with MaxTest=250}  
            "A disjoint with no unions has size subsets" <| 
                fun (djSize: DisjointSetSize) ->
                    let djSet = DisjointSet.make djSize
                    let size = DisjointSet.size djSet
                    let setsCount = subSetsCount djSet
                    
                    setsCount = size            

        testPropertyWithConfig {Config.Quick with MaxTest=250} 
            "A disjoint set has between 0 and size subsets" <|
            fun (disjointSet: DisjointSet) ->
                let setsCount = subSetsCount disjointSet
                let size = DisjointSet.size disjointSet
                
                setsCount >= 0 && setsCount <= size

        testPropertyWithConfig {Config.Quick with MaxTest=250} 
            "A disjoint set has has 1+ subsets if non-zero sized" <|
            fun (disjointSet: DisjointSet) ->
                not (sizeOnePlusCondition disjointSet) || hasOnePlusSubSets disjointSet

        testPropertyWithConfig {Config.Quick with MaxTest=250}
            "Singleton subsets map to themselves." <|
            fun (djSize: DisjointSetSize) ->
                let djSet = DisjointSet.make djSize
                let mappings = subSetMappings djSet
                mappings
                |> Array.mapi (fun index entry -> (index, entry))
                |> Array.forall (fun (index, entry) -> index = (int entry.Index))
                
        testPropertyWithConfig {Config.Quick with MaxTest=250} 
            "An increase size operation updates size to be max of (size and newSize) - never a smaller size" <|
            fun (disjointSet: DisjointSet) (newSize: uint32) ->
                let sizeInitial = DisjointSet.size disjointSet
                DisjointSet.increaseSize disjointSet (Size newSize)
                let sizeUpdated = DisjointSet.size disjointSet

                let newSize' = int newSize                
                if sizeInitial > newSize' then
                    sizeUpdated = sizeInitial
                else 
                    sizeUpdated = newSize'

        testPropertyWithConfig {Config.Quick with MaxTest=250} 
            "An increase size operation does not change the original sub set memberships, only adds new subsets." <|
            fun (disjointSet: DisjointSet) (newSize: uint32) ->
                let mappings = subSetMappings disjointSet                                             
                DisjointSet.increaseSize disjointSet (Size newSize)                
                let mappingsUpdated = subSetMappings disjointSet
                
                mappings = Array.take (Array.length mappings) mappingsUpdated

        testPropertyWithConfig {Config.Quick with MaxTest=250}
            "An increase size operation adds a new subset for each increase in the valid EntryId range." <|
            fun (disjointSet: DisjointSet) (newSize: uint32) ->
                let sizeInitial = DisjointSet.size disjointSet
                let setsCountIntial = subSetsCount disjointSet

                DisjointSet.increaseSize disjointSet (Size newSize)

                let sizeUpdated = DisjointSet.size disjointSet     
                let setsCountUpdated = subSetsCount disjointSet            
                let sizeDiff = sizeUpdated - sizeInitial
                let setsCountDiff = setsCountUpdated - setsCountIntial

                setsCountDiff = sizeDiff

        testPropertyWithConfig {Config.Quick with MaxTest=250} 
            "Union operations fail when out of range and succeed otherwise" <|
            fun (disjointSet: DisjointSet) (u1: EntryId) (u2: EntryId) ->
                let size = DisjointSet.size disjointSet
                let result = DisjointSet.union disjointSet u1 u2
                let shouldSucceed = (int u1.Index) < size && (int u2.Index) < size

                shouldSucceed <> failed result

        testPropertyWithConfig {Config.Quick with MaxTest=250} 
            "Find operations fail when out of range and succeed otherwise" <|
            fun (disjointSet: DisjointSet) (e1: EntryId) ->
                let size = DisjointSet.size disjointSet
                let result = DisjointSet.find disjointSet e1
                let shouldSucceed = (int e1.Index) < size

                shouldSucceed <> failed result
                
        testPropertyWithConfig {Config.Quick with MaxTest=250} 
            "inSameSubset operations fail when out of range and succeed otherwise" <|
            fun (disjointSet: DisjointSet) (e1: EntryId) (e2: EntryId) ->
                let size = DisjointSet.size disjointSet
                let result = DisjointSet.inSameSubset disjointSet e1 e2
                let shouldSucceed = (int e1.Index) < size && (int e2.Index) < size

                shouldSucceed <> failed result      

        testPropertyWithConfig {Config.Quick with MaxTest=250}
            "Out of range failed find, union and inSameSubset operations do not change the disjoint set." <|
            fun (disjointSet: DisjointSet) (outOfBoundsFactor: uint32)->
                let size = DisjointSet.size disjointSet
                let invalidEntryId = (EntryId <| (uint32 size) + outOfBoundsFactor) 
                let mappings = subSetMappings disjointSet

                DisjointSet.union disjointSet invalidEntryId invalidEntryId |> ignore
                DisjointSet.find disjointSet invalidEntryId |> ignore
                DisjointSet.inSameSubset disjointSet invalidEntryId invalidEntryId |> ignore

                let mappingsUpdated = subSetMappings disjointSet                                
                mappings = mappingsUpdated

        testPropertyWithConfig {Config.Quick with MaxTest=250}
            "Unioning a set with itself causes no change." <|
            fun (disjointSet: DisjointSet)  (e: EntryId) ->
                let mappings = subSetMappings disjointSet
                DisjointSet.union disjointSet e e |> ignore
                let mappingsUpdated = subSetMappings disjointSet                                
                mappings = mappingsUpdated

        testPropertyWithConfig {Config.Quick with MaxTest=250}
            "Find does not change the sub set memberships." <|
            fun (disjointSet: DisjointSet)  (e: EntryId) ->
                let mappings = subSetMappings disjointSet
                DisjointSet.find disjointSet e |> ignore
                let mappingsUpdated = subSetMappings disjointSet                                
                mappings = mappingsUpdated
                                
        testPropertyWithConfig {Config.Quick with MaxTest=250} 
            "An entry is in the same subset as itself" <|
            fun (disjointSet: DisjointSet) (e: EntryId) ->
                let result = DisjointSet.inSameSubset disjointSet e e
                match result with 
                | Ok (same, _) -> same 
                | Bad _ -> true // out of range 

        testPropertyWithConfig {Config.Quick with MaxTest=250}
            "A sequence of union and find operations starting with valid entryIds always returns valid EntryIds." <|
            fun (disjointSet: DisjointSet) (ops: Operation list) ->
                let size = DisjointSet.size disjointSet
                let maxIndex = size - 1
                let makeValidEntry = fun _ -> randomEntry maxIndex

                let sequenceOps djSet : bool =          
                           
                    let applyOp entry operation : EntryId = 
                        match operation with 
                        | Find -> DisjointSet.find djSet entry 
                                  |> returnOrFail
                        | Union -> DisjointSet.union djSet (makeValidEntry()) entry  
                                   |> returnOrFail
                                   |> ignore
                                   DisjointSet.find djSet entry 
                                   |> returnOrFail

                    let firstRandEntry = makeValidEntry()                        
                    let finalEntry = List.fold applyOp firstRandEntry ops
                    entryValid finalEntry maxIndex

                isEmpty disjointSet || sequenceOps disjointSet

        testPropertyWithConfig {Config.Quick with MaxTest=250}
            "After a successful union of a b then a b are in the same subset." <|
            fun (disjointSet: DisjointSet) ->

                let checkInSameSubsetAfterUnion = fun _ ->
                    let size = DisjointSet.size disjointSet
                    let maxIndex = size - 1
                    let makeValidEntry = fun _ -> randomEntry maxIndex          
                
                    let a = makeValidEntry()
                    let b = makeValidEntry()
                    DisjointSet.union disjointSet a b 
                    |> returnOrFail
                    |> ignore
                
                    DisjointSet.inSameSubset disjointSet a b
                    |> returnOrFail

                isEmpty disjointSet || checkInSameSubsetAfterUnion()

        testPropertyWithConfig {Config.Quick with MaxTest=250}
            "After a union of a b then at most one of a b change their subset EntryId to be the same subset." <|
            fun (disjointSet: DisjointSet) ->

                let checkUnioningBetweenCorrectSubsets = fun _ ->
                    let size = DisjointSet.size disjointSet
                    let maxIndex = size - 1
                    let makeValidEntry = fun _ -> randomEntry maxIndex          
                
                    let a = makeValidEntry()
                    let b = makeValidEntry()
                    let a_set = DisjointSet.find disjointSet a |> returnOrFail
                    let b_set = DisjointSet.find disjointSet b |> returnOrFail

                    DisjointSet.union disjointSet a b 
                    |> returnOrFail
                    |> ignore
                
                    let a_setUpdated = DisjointSet.find disjointSet a |> returnOrFail
                    let b_setUpdated = DisjointSet.find disjointSet b |> returnOrFail

                    // Testing elsewhere that they are the same subset
                    let a_inAB = (a_setUpdated = a_set) || (a_setUpdated = b_set)
                    let b_inAB = (b_setUpdated = a_set) || (b_setUpdated = b_set)
                    
                    a_inAB && b_inAB

                isEmpty disjointSet || checkUnioningBetweenCorrectSubsets()

        testPropertyWithConfig {Config.Quick with MaxTest=250}
            "Unions between entries of different subsets reduce the subset count by 1." <|
            fun (disjointSet: DisjointSet) ->

                let checkSubSetCountReduction = fun _ ->
                    let size = DisjointSet.size disjointSet
                    let maxIndex = size - 1
                    let makeValidEntry = fun _ -> randomEntry maxIndex
                    let setsCount = subSetsCount disjointSet

                    let a = makeValidEntry()
                    let b = makeValidEntry()
                    let sameSubSet = DisjointSet.inSameSubset disjointSet a b
                                     |> returnOrFail

                    DisjointSet.union disjointSet a b 
                    |> returnOrFail
                    |> ignore

                    let setsCountUpdated = subSetsCount disjointSet

                    if sameSubSet then 
                        setsCount = setsCountUpdated
                    else
                        setsCountUpdated = setsCount - 1

                isEmpty disjointSet || checkSubSetCountReduction()

    ]
    