#I "../../packages"
#r @"Streams/lib/net45/Streams.dll" 
#r @"FAKE/tools/FakeLib.dll"
#load "Result.fs"
#load "DomainTypes.fs"
#load "Graph.fs"
#load "Generation.fs"
#load "Algorithms.fs"
#load "Visualisation.fs"

open System
open System.Collections.Generic 
open System.IO

open Graphs


let outdir = __SOURCE_DIRECTORY__ // Places for generated graphs on the file system

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

let makeShowGraphViz (graph: Graph) vizName = 
    let tryOpenVizFile fileName = 
        match fileName with
        | Some(f) -> shellOpenFileWithDefaultApplication f |> ignore
        | _ -> ()
    Visualisation.toDotGraphDescriptionLanguage graph
    |> Visualisation.makeGraphVisualisation <| Path.Combine(outdir, vizName)
    |> tryOpenVizFile

let home = Environment.GetEnvironmentVariable("HOME")
let test_graph_file file_name = 
    let dir = Path.Combine(__SOURCE_DIRECTORY__, @"../../tests/Graphs.Tests/") 
    Path.Combine(dir, file_name) |> Path.GetFullPath

let dag_file = test_graph_file "directed_graph.txt"
let g_dag = Generation.readGraphFromFile dag_file true

let undirected_file = test_graph_file "undirected_graph.txt"
let g_undir = Generation.readGraphFromFile undirected_file false

let maze_graph_file = Path.Combine(home, "dev/rust/mazes/le-graph.text")
let g_maze = Generation.readGraphFromFile maze_graph_file false

let ssc_file = test_graph_file "strong_components_graph.txt"
let g_scc = Generation.readGraphFromFile ssc_file true

let linear_file = test_graph_file "linearly_ordered_graph.txt"
let g_linear = Generation.readGraphFromFile linear_file true

Algorithms.reverseDirectedGraph g_dag
Algorithms.isDAG g_dag
Algorithms.dfsPrePostOrderNumbers g_dag
Algorithms.edgesSet g_dag
Algorithms.stronglyConnectedComponents g_dag
Algorithms.topologicalOrdering g_dag

Visualisation.toDotGraphDescriptionLanguage g_undir

let rev_scc = Algorithms.reverseDirectedGraph g_scc |> Option.get
let s_comps = Algorithms.stronglyConnectedComponents g_scc |> Option.get


makeShowGraphViz rev_scc "reverse_strong_components"
makeShowGraphViz g_scc "strong_components"
makeShowGraphViz g_dag "dag"
makeShowGraphViz g_undir "undir"