System.IO.Directory.SetCurrentDirectory (__SOURCE_DIRECTORY__)
#I "../../packages"
#I "../../src/Graphs"
#r @"Chessie/lib/net40/Chessie.dll"
#r @"test/FsCheck/lib/net45/FsCheck.dll"
#load "Heaps.fs"

open System
open FsCheck
open Graphs.Heaps

[<StructuredFormatDisplay("FooBar Heap={Heap} Arity={TreeNodeChildCount}")>]
type FooBar<'T> = {
    Heap: ResizeArray<'T>
    TreeNodeChildCount: int
}

let heapGen = Arb.generate<DHeap.DHeap<int>>
Gen.sample 8 2 heapGen

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


module Test1 =
    [<AutoOpen>]
    module FooModule =     // why bother with this wrapper when it is auto-opened?
        type Foo = private { A: int; B: bool }
        [<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
        module Foo = 
            let make n = {A=n; B=true}
    type Foo = FooModule.Foo  // after opening Test1 we can do Foo.make 11  => FSI_0002+Test1+FooModule+Foo
    // without this still Foo.make 11 => FSI_0002+Test1+FooModule+Foo

module Test2 =
    
    type Foo = private { A: int; B: bool }
    [<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module Foo = 
        let make n = {A=n; B=true}
    //type Foo = FooModule.Foo