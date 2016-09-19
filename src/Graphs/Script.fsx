#I "../../packages"
#r @"Streams/lib/net45/Streams.dll" 
#r @"FAKE/tools/FakeLib.dll"
#load "DomainTypes.fs"
#load "Generation.fs"
#load "Algorithms.fs"
#load "Visualisation.fs"

open System
open System.Collections.Generic 
open System.IO

open Graphs


let home = Environment.GetEnvironmentVariable("HOME")
let maze_graph_file = Path.Combine(home, "dev/rust/mazes/le-graph.text")
let test_graph_file file_name = 
    let dir = Path.Combine(__SOURCE_DIRECTORY__, @"../../tests/Graphs.Tests/") 
    Path.Combine(dir, file_name) |> Path.GetFullPath     
let dag_file = test_graph_file "directed_graph.txt"
let undirected_file = test_graph_file "undirected_graph.txt"
let g_dag = Generation.readGraph dag_file true
let g_undir = Generation.readGraph undirected_file false
let g_maze = Generation.readGraph maze_graph_file false

Algorithms.reverseDirectedGraph g_dag
Algorithms.isDAG g_dag
Algorithms.dfsPrePostOrderNumbers g_dag
Algorithms.edgesSet g_dag

Visualisation.toDotGraphDescriptionLanguage g_undir
let outdir = __SOURCE_DIRECTORY__
let outFile = Path.Combine(outdir, "dag")
let dag_dot_definition = Visualisation.toDotGraphDescriptionLanguage g_dag
let vizFile = Visualisation.makeGraphVisualisation dag_dot_definition outFile

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
