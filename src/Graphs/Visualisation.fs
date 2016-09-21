namespace Graphs

open System.IO

module Proc = Fake.ProcessHelper


module Visualisation = 

    open Algorithms 

    let toDotGraphDescriptionLanguage (graph: Graph) = 
        let descriptionOpen = 
            if graph.IsDirected then
                "digraph {"
            else
                "graph {"
        let descriptionClose = "}"
        
        let edgeToString = 
            let edgeSyntax = 
                if graph.IsDirected then
                    " -> "
                else 
                    " -- "
            (fun (v1, v2) -> 
                string v1 + edgeSyntax + string v2)

        let edges = edgesSet graph
        let edgeDescriptions = edges 
                               |> Seq.map edgeToString
                               |> Seq.map (fun s -> "    " + s)

        seq { yield descriptionOpen
              yield! edgeDescriptions
              yield descriptionClose}
        |> String.concat "\n"


    let makeGraphVisualisation dotDescription outFilePathNoExtension = 
        
        let outDir = Path.GetDirectoryName(outFilePathNoExtension)

        if not (Directory.Exists outDir) then 
            failwith <| sprintf "Directory of the output file does not exist: %s" outDir
        
        let dotTempFileName = Path.GetTempFileName()
        File.WriteAllText(dotTempFileName, dotDescription)
        
        let dotCmd = "dot"
        let dotVisualisationFile = outFilePathNoExtension + ".png"
        let dotArgs = "-Tpng " + dotTempFileName + " -o " + dotVisualisationFile

        try
            Proc.Shell.Exec(dotCmd, dotArgs, outDir) |> ignore
            Some(dotVisualisationFile)
        with 
            | :? System.ComponentModel.Win32Exception as e ->
                    printfn """Failed to run graph visual creation process "%s".\n%A""" (dotCmd + " " + dotArgs) e
                    None


