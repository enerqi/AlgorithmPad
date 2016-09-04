// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "Graphs.fs"

open Graphs
open System
open System.Collections.Generic 
open System.IO

let g = Graphs.readGraph @"C:\Users\Enerqi\dev\rust\mazes\le-graph.text";;
