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