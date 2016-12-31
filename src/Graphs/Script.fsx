#I "../../packages"
#r @"Chessie/lib/net40/Chessie.dll"
#r @"FAKE/tools/FakeLib.dll"
#r @"Streams/lib/net45/Streams.dll" 
#load "Result.fs"
#load "Heaps.fs"
#load "DisjointSet.fs"
#load "DomainTypes.fs"
#load "ResultHandling.fs"
#load "Graph.fs"
#load "Generation.fs"
#load "Algorithms.fs"
#load "Visualisation.fs"

open System
open System.Collections.Generic 
open System.IO

open Chessie.ErrorHandling

open Graphs


let outdir = __SOURCE_DIRECTORY__ // Places for generated graphs on the file system

/// Use the system shell (command prompt) to open the file - F# interactive only
let shellOpenFileWithDefaultApplication (fileName: string) : GraphResult<int> =
    // Don't wait on the process - use START to run the shell command in a separate ignored process
    let setupProcess = 
        (fun (processStartInfo : Diagnostics.ProcessStartInfo) -> 
                processStartInfo.FileName <- "cmd.exe"
                processStartInfo.Arguments <- "/C START " + fileName)
    // We could also use Proc.asyncShellExec, but we don't wait on the command
    // it's really only the start up cost of cmd.exe that we are blocked on.
    tryF (fun _ -> Fake.ProcessHelper.ExecProcess setupProcess (TimeSpan.FromSeconds 2.0))
         FileAccessFailure     

let makeShowGraphViz (vizName: string) (graph: Graph) : GraphResult<string> =             
    trial {
        let! graphDef = Visualisation.toDotGraphDescriptionLanguage graph
        let outFilePathNoExtension = Path.Combine(outdir, vizName)
        let! imagePath = Visualisation.makeGraphVisualisation graphDef outFilePathNoExtension 
        let! processExitCode = shellOpenFileWithDefaultApplication imagePath
        return imagePath
    }


let home = Environment.GetEnvironmentVariable("HOME")
let test_graph_file (file_name: string) : string = 
    let dir = Path.Combine(__SOURCE_DIRECTORY__, @"../../tests/Graphs.Tests/resources/") 
    Path.Combine(dir, file_name) |> Path.GetFullPath

let dag_file = test_graph_file "directed_graph.txt"
let g_dag = Generation.readGraphFromFile true dag_file

let undirected_file = test_graph_file "undirected_graph.txt"
let g_undir = Generation.readGraphFromFile false undirected_file 

let maze_graph_file = Path.Combine(home, "dev/rust/mazes/le-graph.text")
let g_maze = Generation.readGraphFromFile false maze_graph_file 

let ssc_file = test_graph_file "strong_components_graph.txt"
let g_scc = Generation.readGraphFromFile true ssc_file

let linear_file = test_graph_file "linearly_ordered_graph.txt"
let g_linear = Generation.readGraphFromFile true linear_file 

let weighted_file = test_graph_file "weighted_undirected_graph.txt"
let g_weighted = Generation.readGraphFromFile false weighted_file

let g_web = Generation.readGraphFromFile true <| test_graph_file "web_directed_graph.txt"

lift Algorithms.reverseDirectedGraph g_dag
lift Algorithms.isDAG g_dag
lift Algorithms.dfsPrePostOrderNumbers g_dag
lift Algorithms.edgesSet g_dag
lift Algorithms.stronglyConnectedComponents g_dag
lift Algorithms.topologicalOrdering g_dag

lift Visualisation.toDotGraphDescriptionLanguage g_undir
lift Visualisation.toDotGraphDescriptionLanguage g_weighted

let rev_scc = g_scc |> lift Algorithms.reverseDirectedGraph
let s_comps = g_scc |> lift Algorithms.stronglyConnectedComponents

rev_scc |> lift (makeShowGraphViz "reverse_strong_components")
g_scc   |> lift (makeShowGraphViz  "strong_components")
g_dag   |> lift (makeShowGraphViz "dag")
g_undir |> lift (makeShowGraphViz "undir")
g_weighted |> lift (makeShowGraphViz "weighted")
//g_web |> lift (makeShowGraphViz "web")
