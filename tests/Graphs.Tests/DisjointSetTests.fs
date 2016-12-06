module Graph.DisjointSetTests

open Chessie.ErrorHandling
open FsCheck
open FsCheck.GenBuilder
open Fuchu
open FsUnit

open Graphs

module DisjointSetTestUtils =

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


open DisjointSetTestUtils

[<Tests>]
let disjointSetTests = 
    Arb.register<DisjointSetGenerators>() |> ignore

    testList "DisjointSet ADT" [

    ]
    