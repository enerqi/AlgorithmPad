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

    let sizeOnePlusCondition (disjointSet: DisjointSet) : bool =
        DisjointSet.size disjointSet > 0

    let hasOnePlusSubSets (disjointSet: DisjointSet) : bool = 
        let sets = subSets disjointSet
        let setsCount = Array.length sets
        let size = DisjointSet.size disjointSet
        setsCount >= 1 && setsCount <= size        

    let entryValid (e: EntryId) (maxSize: int) : bool = 
        (int e.Index) < maxSize

    let randomEntry (maxIndex: int) : EntryId = 
        Gen.choose(0, maxIndex) |> Gen.sample 0 1 |> List.head |> uint32 |> EntryId
            

open DisjointSetTestUtils

[<Tests>]
let disjointSetTests = 
    Arb.register<DisjointSetGenerators>() |> ignore

    testList "DisjointSet ADT" [

        testPropertyWithConfig {Config.Quick with MaxTest=1000} 
            "A disjoint set has between 0 and size subsets" <|
            fun (disjointSet: DisjointSet) ->
                let sets = subSets disjointSet
                let setsCount = Array.length sets
                let size = DisjointSet.size disjointSet
                setsCount >= 0 && setsCount <= size

        testPropertyWithConfig {Config.Quick with MaxTest=1000} 
            "A disjoint set has has 1+ subsets if non-zero sized" <|
            fun (disjointSet: DisjointSet) ->
                not (sizeOnePlusCondition disjointSet) || hasOnePlusSubSets disjointSet

        testPropertyWithConfig {Config.Quick with MaxTest=1000} 
            "An increase size operation updates size to be max (size newSize) - never a smaller size" <|
            fun (disjointSet: DisjointSet) (newSize: uint32) ->
                let sizeInitial = DisjointSet.size disjointSet
                DisjointSet.increaseSize disjointSet (Size newSize)
                let sizeUpdated = DisjointSet.size disjointSet

                let newSize' = int newSize                
                if sizeInitial > newSize' then
                    sizeUpdated = sizeInitial
                else 
                    sizeUpdated = newSize'

        testPropertyWithConfig {Config.Quick with MaxTest=1000}
            "An increase size operation adds a new subset for each increase in the valid EntryId range." <|
            fun (disjointSet: DisjointSet) (newSize: uint32) ->
                let sizeInitial = DisjointSet.size disjointSet
                let setsInitial = subSets disjointSet   
                let setsCountIntial = Array.length setsInitial

                DisjointSet.increaseSize disjointSet (Size newSize)

                let sizeUpdated = DisjointSet.size disjointSet     
                let setsUpdated = subSets disjointSet              
                let setsCountUpdated = Array.length setsUpdated

                let sizeDiff = sizeUpdated - sizeInitial
                let setsCountDiff = setsCountUpdated - setsCountIntial
                setsCountDiff = sizeDiff

        testPropertyWithConfig {Config.Quick with MaxTest=250} 
            "Union operations fail when out of range" <|
            fun (disjointSet: DisjointSet) (u1: EntryId) (u2: EntryId) ->
                let size = DisjointSet.size disjointSet
                let result = DisjointSet.union disjointSet u1 u2
                let shouldSucceed = (int u1.Index) < size && (int u2.Index) < size

                failed result <> shouldSucceed

        testPropertyWithConfig {Config.Quick with MaxTest=250} 
            "Find operations fail when out of range" <|
            fun (disjointSet: DisjointSet) (e1: EntryId) ->
                let size = DisjointSet.size disjointSet
                let result = DisjointSet.find disjointSet e1
                let shouldSucceed = (int e1.Index) < size

                failed result <> shouldSucceed           
                
        testPropertyWithConfig {Config.Quick with MaxTest=250} 
            "inSameSubset operations fail when out of range" <|
            fun (disjointSet: DisjointSet) (e1: EntryId) (e2: EntryId) ->
                let size = DisjointSet.size disjointSet
                let result = DisjointSet.inSameSubset disjointSet e1 e2
                let shouldSucceed = (int e1.Index) < size && (int e2.Index) < size

                failed result <> shouldSucceed            

        testPropertyWithConfig {Config.Quick with MaxTest=250}
            "Unioning a set with its self causes no change." <|
            fun (disjointSet: DisjointSet)  (e: EntryId) ->
                let mappings = subSetMappings disjointSet
                DisjointSet.union disjointSet e e |> ignore
                let mappings' = subSetMappings disjointSet                                
                mappings = mappings'
                                
        testPropertyWithConfig {Config.Quick with MaxTest=250} 
            "An entry is always in the same subset as itself" <|
            fun (disjointSet: DisjointSet) (e: EntryId) ->
                let result = DisjointSet.inSameSubset disjointSet e e
                match result with 
                | Ok (same, _) -> same 
                | Bad _ -> true

        testPropertyWithConfig {Config.Quick with MaxTest=250}
            "A sequence of union and find operations starting with valid entryIds always returns valid EntryIds." <|
            fun (disjointSet: DisjointSet) (ops: Operation list) ->
                let size = DisjointSet.size disjointSet
                let maxIndex = size - 1
                let makeValidEntry = fun _ -> randomEntry maxIndex

                let sequenceOps operations : bool =                     
                    let applyOp entry operation : EntryId = 
                        match operation with 
                        | Find -> DisjointSet.find disjointSet entry 
                                  |> returnOrFail
                        | Union -> DisjointSet.union disjointSet entry (makeValidEntry()) 
                                   |> returnOrFail
                                   |> ignore
                                   DisjointSet.find disjointSet entry 
                                   |> returnOrFail
                        
                    let finalEntry = List.fold applyOp (makeValidEntry()) operations
                    entryValid finalEntry maxIndex

                not (hasOnePlusSubSets disjointSet) || sequenceOps ops
                
                
        // [- any union/find/sameSubset failure does not change the subSetMappings]
        // [- size increase does not change the subSetMappings, but does add new subsets]
        // - each union between different subsets decreases the subset count (union a b if not samesubset a b)
        // - after union a b, then same subset a b
    ]
    