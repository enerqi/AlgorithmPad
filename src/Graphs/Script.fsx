// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "Graphs.fs"

open Graphs
open System
open System.Collections.Generic 
open System.IO

let home = Environment.GetEnvironmentVariable("HOME")
let graph_file = Path.Combine(home, "dev/rust/mazes/le-graph.text")

let g = Graphs.readGraph graph_file
