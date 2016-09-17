#I "../../packages"
#r @"Streams/lib/net45/Streams.dll" 
#r @"FAKE/tools/FakeLib.dll"

#load "Graphs.fs"

open Graphs
open GraphViz
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
Graphs.edgesSet g_dag

GraphViz.toDotGraphDescriptionLanguage g_undir
let outdir = __SOURCE_DIRECTORY__
let outFile = Path.Combine(outdir, "dag")
let dag_dot_definition = GraphViz.toDotGraphDescriptionLanguage g_dag
let vizFile = GraphViz.makeGraphVisualisation dag_dot_definition outFile

/// Use the system shell (command prompt) to open the file - F# interactive only
let shellOpenFileWithDefaultApplication fileName =
    // Don't wait on the process - use START to run the shell command in a separate ignored process
    let setupProcess = 
        (fun (processStartInfo : Diagnostics.ProcessStartInfo) -> 
                processStartInfo.FileName <- "cmd.exe"
                processStartInfo.Arguments <- "/C START " + fileName)
    // We could also use Proc.asyncShellExec, but we don't wait on the command
    // it's really only the start up cost of cmd.exe that we are blocked on.
    Fake.ProcessHelper.ExecProcess setupProcess (TimeSpan.FromSeconds 2.0) 

shellOpenFileWithDefaultApplication vizFile
