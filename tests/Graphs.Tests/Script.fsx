System.IO.Directory.SetCurrentDirectory (__SOURCE_DIRECTORY__)
#I "../../packages"
#I "../../src/Graphs"
#r @"Chessie/lib/net40/Chessie.dll"
#r @"test/FsCheck/lib/net45/FsCheck.dll"
#load "Heaps.fs"

open System
open FsCheck
open Graphs.Heaps

// Arb.generate - the *generator* of the registered arbitrary instance for that type
// Not so useful as it uses reflection and directly creates impossible arities and heaps
let heapGen = Arb.generate<DHeap<int>>
Gen.sample 8 2 heapGen

Gen.sample 128 10 Arb.generate<Capacity> // a good range of silly values + some defaults 
Gen.sample 10 10 Arb.generate<HeapArity> // fine, biased towards binary
Gen.sample 10 10 Arb.generate<HeapRootOrdering> // fine

type DHeapParams = { 
    Capacity: Capacity
    Arity: HeapArity
    Order: HeapRootOrdering
}
Gen.sample 128 10 Arb.generate<DHeapParams> 

Arb.from<int> // what arb instance is used for this type
Arb.from<DHeap<float>>
Arb.fromGen  // creating instances with custom generator for type
Arb.fromGenShrink // but not registered unless call Arb.register
Arb.register
Arb.registerByType

// So, need to build a custom Gen (+ shrinker) and pass to Arb.fromGen then Arb.register that arbitrary
// Or need to build a custom gen + shrinker and add to Arbitrary<'a> subclass trick
// The custom gen needs to Gen DHeapParams as a type or individual types and combine into constructor call

Gen.oneof // randomly choose one of these generators

open FsCheck.GenBuilder // gen computation expression
let genHeap : Gen<DHeap<int>> = // a generator of DHeap
    gen {        
        let! arity = Arb.generate<HeapArity>

        //let! order = Gen.oneof [ gen { return MinKey }; gen { return MaxKey }]
        let! order = Arb.generate<HeapRootOrdering>
        // same as
        //let! order = Gen.frequency  [ (1, gen { return MinKey }); (1, gen { return MaxKey }) ]        
        
        //let cap = Gen.sample 128 1 Arb.generate<Capacity> |> List.head
        let! cap = Arb.generate<Capacity>

        let! contents = Gen.listOf Arb.generate<int>

        if List.length contents > 0 then
            return DHeap.ofSeq arity order cap (Seq.ofList contents)
        else
            return DHeap.empty arity order cap
    }
type HeapGenerators = 
    static member DHeap() =
        { new Arbitrary<DHeap<int>>() with
              override this.Generator = genHeap
              override this.Shrinker t = Seq.empty }
Arb.register<HeapGenerators>() 

// no shrinker
let heapArb: Arbitrary<DHeap<int>> = Arb.fromGen genHeap

Gen.sample 10 10 genHeap
Gen.sample 0 10 Arb.generate<DHeap<int>>



// we could make a shrinker that changes arity down to binary. 
//let shrinkHeap: (DHeap<IComparable> -> seq<DHeap<IComparable>>) = seq {return [] }
//let heapArbWithShrinker: Arbitrary<DHeap<IComparable>> = Arb.fromGenShrink genHeap shrinkHeap


// what about a DHeap with contents? Access `sized` info and use ofSeq on the 'T type
// to be generated with Gen.sample thesize count Arb.generate<'T>?
// how to control the length or that is done for us...?
// maybe it should just be the one generator and if size is zero then so be it empty?
// 

// ?
//type MyCustomThingy = 
//    static member Blah { new Arbitrary<Blah> ... }
//Arb.register<MyCustomThingy>()
//Gen.sample 10 10 Arb.generate<MyCustomThingy> 

// The computation expression 'gen' can build a custom generator as can Gen module functions.
// Shrinkers are 'a -> seq<'a> - just use core seq {...} computation expression etc.
// An Arbitrary<'a> instance packages a generator and shrinker together to be used in properties. 
// FsCheck also allows you to register Arbitrary instances in a Type to Arbitrary dictionary. 
// This dictionary is used to find an arbitrary instance for properties that have arguments, 
// based on the argument's type.




// Note fuchu testPropertyWithConfig just uses a config and calls FsCheck.Check.One
// as a testCase via callling `testProperty`
//let testPropertyWithConfig (config: Config) name property = 
//        let config =
//            { config with
//                Runner = wrapRunner config.Runner }
//        testCase name <|
//            fun _ ->
//                ignore Runner.init.Value
//                FsCheck.Check.One(name, config, property)
    
//let testProperty name = testPropertyWithConfig config name


let intGen = Arb.generate<int>
Gen.sample 3 10 intGen
Gen.sample 100 5 intGen
// list and option compound types come for free
Gen.sample 100 5 Arb.generate<int list> // 5 x int lists
Gen.sample 100 5 Arb.generate<int option> // 5 x option<int>

type Color = Red | Green of int | Blue of bool
Gen.sample 10 42 Arb.generate<Color> // Red or Blue true|false or Green max 42

let isSmaller80 x = x < 80
Check.Quick isSmaller80
Check.One({Config.Quick with MaxTest = 1000}, isSmaller80)

// startsize, endsize paramaters to config
// default 1, 100. But probabilities distribute the numbers towards zero
// so this falsifies quicker
Check.One({Config.Quick with EndSize = 1000}, isSmaller80)

Check.Verbose isSmaller80

// except for 0 0 
let preCondition x y =
    (x, y) <> (0, 0)
let additionIsNotMultiplication x y =
    x + y <> x * y
let addIsNotMulExceptZero x y =
    preCondition x y ==> additionIsNotMultiplication x y
// This kind of precondition should only be used if you want to filter out a small number of cases.
Check.Quick addIsNotMulExceptZero

// Combining properties
let add x y = x + y // good implementation
let commutativeProperty x y = 
    add x y = add y x    
let associativeProperty x y z = 
    add x (add y z) = add (add x y) z    
let leftIdentityProperty x = 
    add x 0 = x
let rightIdentityProperty x = 
    add 0 x = x
type AdditionSpecification =
    static member ``Commutative`` x y = commutativeProperty x y
    static member ``Associative`` x y z = associativeProperty x y z 
    static member ``Left Identity`` x = leftIdentityProperty x 
    static member ``Right Identity`` x = rightIdentityProperty x 
    // some examples as well - so seems less abstract
    static member ``1 + 2 = 3``() =  
        add 1 2 = 3
    static member ``1 + 2 = 2 + 1``() =  
        add 1 2 = add 2 1 
    static member ``42 + 0 = 0 + 42``() =  
        add 42 0 = add 0 42 

Check.QuickAll<AdditionSpecification>()
