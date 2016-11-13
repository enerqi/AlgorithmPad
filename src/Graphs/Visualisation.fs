namespace Graphs

open System.IO

module Proc = Fake.ProcessHelper


module Visualisation = 

    open System
    open Chessie.ErrorHandling
    open Algorithms 

    /// Transform a graph to a string that is a valid dot graph description language of it
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


    /// Run the external 'dot' process to generate a graph image file
    /// Returns a Result of the image path or Error if creation failed
    let makeGraphVisualisation dotDescription outFilePathNoExtension = 
        
        // IO string (dir name)
        let getOutFileDirectory = fun _ ->
            Path.GetDirectoryName(outFilePathNoExtension)
        
        // We return a result and use >>=
        // or return a value | throw the exception and use tryF around it
        // or return a value an just use lift
        // How to thread/append the new state through without stacking up ugly tuples?
        // It might be nice to create a command and not do a side effect until done all pure work
        // ...
        // tryF is also meant to be a unit function so need to partial apply the result/state?
        // or rather it is another successTee function? Tee will not change the result (but can do any side effect)
        //  
        // interesting that haskell do syntax sugar essentially nests bind like functions so a later function in 
        // the chain can ref the state in the outer function, it does not need threading through as an additional
        // parameter
        // this is perhaps where we should look at the `trial` computation expression! Instead of these 
        // ever growing tuples of local state.

        // dirName -> Result IO string or Result IO () - unit meaning all went well
        let checkDirExists outDir = 
            if not (Directory.Exists outDir) then 
                let exn = new ArgumentException("outFilePathNoExtension", 
                                                sprintf "Directory of out file (%s) does not exist" outDir)
                fail (FileAccessFailure exn)
            else
                ok outDir

        // could go first as does not need any input state, but it should not be done really until needed
        let makeTempFile =
            Path.GetTempFileName()

        // templfile+dirName+[description] -> IO string (temp file name)
        let writeDotTempFile = fun _ ->
            let dotTempFileName = Path.GetTempFileName()
            File.WriteAllText(dotTempFileName, dotDescription)
            ok (outDir, dotTempFileName)

        // tempFileName -> string * string * string
        let processSetup dotTempFileName = 
            let dotCmd = "dot"
            let dotVisualisationFile = outFilePathNoExtension + ".png"
            let dotArgs = "-Tpng " + dotTempFileName + " -o " + dotVisualisationFile
            (dotCmd, dotVisualisationFile, dotArgs)
          
        let runProcess = 
            try
                Proc.Shell.Exec(dotCmd, dotArgs, outDir) |> ignore
                Some(dotVisualisationFile)
            with 
                | :? System.ComponentModel.Win32Exception as e ->
                        printfn """Failed to run graph visual creation process "%s".\n%A""" (dotCmd + " " + dotArgs) e
                        None                          

        tryF getOutFileDirectory FileAccessFailure
        >>= checkDirExists
        |> tryF writeDotTempFile FileAccessFailure
        
        
        //let dotTempFileName = Path.GetTempFileName()
        //File.WriteAllText(dotTempFileName, dotDescription)
        
        //let dotCmd = "dot"
        //let dotVisualisationFile = outFilePathNoExtension + ".png"
        //let dotArgs = "-Tpng " + dotTempFileName + " -o " + dotVisualisationFile

        


