namespace Graphs

// F# 4.1 will add the Result type to FSharp.Core
// In some ways it is just the `Choice` type, but with clearer semantics
// See https://github.com/fsprojects/Chessie/blob/master/src/Chessie/ErrorHandling.fs
// for other possible functions to add to the Result type or module even including a 
// computation expression `trial`
type Result<'TSuccess, 'TError> = 
    | Ok of 'TSuccess
    | Error of 'TError
    
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>] // will be implicit in F# 4.1 
module Result =    

    //---------- These are the uncontentious functions that will definitely be part of FSharp.Core
    [<CompiledName("Map")>]
    let map (f: 'T -> 'U) (inp: Result<'T, 'TError>): Result<'U, 'TError> = 
        match inp with 
        | Error e -> Error e 
        | Ok x -> Ok (f x)

    [<CompiledName("MapError")>]
    let mapError (f: 'TError -> 'U) (inp: Result<'T, 'TError>) : Result<'T, 'U>  = 
        match inp with 
        | Error e -> Error (f e) 
        | Ok x -> Ok x

    [<CompiledName("Bind")>]
    let bind (f: 'T -> Result<'U, 'TError>) (inp: Result<'T, 'TError>) : Result<'U, 'TError>= 
        match inp with 
        | Error e -> Error e 
        | Ok x -> f x
