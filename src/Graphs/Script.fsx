// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "Library.fs"
open Graphs

let num = Graphs.readGraph 42
printfn "%i" num

(*
4 5
2 1
4 3
1 4
2 4
3 2
*)

open System;; 
open System.IO;; 

let fdir = @"C:\Users\Enerqi\dev\fsharp\Graphs\src";;
let fname = "undirected_graph.txt";;
let f = Path.Combine(fdir, fname);;
let dataLines = File.ReadLines(f);;
// filter "empty" lines

let header, edges = dataLines.[0], dataLines.[1..];;
let verticesCount, edgesCount = header.Split() |> Array.map int;;

let extractVertexPair (pairString: string) = 
    match pairString.Split() with
    | [|v1; v2|] -> (int v1, int v2)
        // Todo: Improve error handling
    | _ -> failwith "Failed to extract pair from vertex pair string: %s" pairString
     
type Thing = {
    S: string
}

type Foo = { // foo is immutable,
    A: int
    AS: Thing array // array is the .net BCL resizable list (c++ vector). The ref is final but the data is mutable.
}


let ra = new ResizeArray<Object>() 