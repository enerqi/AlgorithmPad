// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.
#I "../../packages"
#r "Streams/lib/net45/Streams.dll"  // better way to not refer to specific .net version number?

#load "Graphs.fs"


open Graphs
open System
open System.Collections.Generic 
open System.IO

let home = Environment.GetEnvironmentVariable("HOME")
let maze_graph_file = Path.Combine(home, "dev/rust/mazes/le-graph.text")

let test_graph_file file_name = 
    let dir = Path.Combine(__SOURCE_DIRECTORY__, @"../../tests/Graphs.Tests/") 
    Path.Combine(dir, file_name) |> Path.GetFullPath 
    
let dag_file = test_graph_file "directed_graph.txt"
let undirected_file = test_graph_file "undirected_graph.txt"

let g_dag = Graphs.readGraph dag_file true
let g_undir = Graphs.readGraph undirected_file false
let g_maze = Graphs.readGraph maze_graph_file false


Graphs.reverseDirectedGraph g_dag

Graphs.isDAG g_dag

Graphs.dfsPrePostOrderNumbers g_dag

