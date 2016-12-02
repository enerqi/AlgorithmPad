namespace Graphs

[<AutoOpen>]
module DisjointSetModule = 

    
    type [<NoEquality; NoComparison>]  DisjointSet<'T> = private {
        ArrayBackedForest: ResizeArray<int>
        // symbol table usage in the API? 'T -> int|uint
        // do we want a trivial api? :
        // `create of size n`, `find`, `union` only?
        // find int private and find 'T public
        // union int int private and union 'T 'T public
    }

    [<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module DisjointSet = 

        let foo = 
            1

type DisjointSet<'T> = DisjointSetModule.DisjointSet<'T>

