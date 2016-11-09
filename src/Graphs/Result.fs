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

    
    //---------- Extra result functions not guaranteed for the 4.1 API
    
    // If the wrapped function is a success and the given result is a success the function is applied on the value. 
    // Otherwise the exisiting error is propagated.
    let inline apply wrappedFunction result = 
        match wrappedFunction, result with
        | Ok(f), Ok(x) -> Ok(f x)
        | Error e, Ok(_) -> Error(e)
        | Ok(_), Error e -> Error(e)
        | Error e, Error e2 -> Error e // arbitrary, maybe the result type should use lists?

    // Lifts a function into a Result container and applies it on the given result.
    let inline lift f result = apply (Ok f) result

    // Collects a sequence of Results and accumulates their values.
    // If the sequence contains an error the error will be propagated.
    let inline collect xs = 
        Seq.fold (fun result next -> 
            match result, next with
            | Ok(a), Ok(rs) -> Ok(a :: rs)
            | Ok(_), Error e 
            | Error e, Ok(_) -> Error e
            | Error e, Error e2 -> (Error e)) (ok []) xs
        |> lift List.rev