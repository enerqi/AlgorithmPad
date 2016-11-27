namespace Graphs

open Chessie.ErrorHandling

// F# 4.1 will add the Result type to FSharp.Core
// In some ways it is just the `Choice` type, but with clearer semantics
// For now we use Chessie as it comes with utility functions that even F# 4.1 core may not ship with
// See https://github.com/fsprojects/Chessie/blob/master/src/Chessie/ErrorHandling.fs
[<AutoOpen>]    
module Result =    

    /// Run the unit input function `f` and return its Result if successful else
    /// convert any exceptions to the error type with the given `exnToMessage` function.
    let inline tryF (f: unit -> 'a) (exnToMessage: _ -> 'b) : Result<'a, 'b>= 
        try
            f() |> ok
        with 
            exn -> fail (exnToMessage exn)
