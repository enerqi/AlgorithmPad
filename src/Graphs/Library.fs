namespace Graphs

open System
open System.IO

/// Documentation for my library
///
/// ## Example
///
///     let h = Library.hello 1
///     printfn "%d" h
///
module Graphs = 

  type VertexId = VId of int  

  type Vertex<'a> = {
    //Standard F# types (discriminated unions, records, lists, arrays and tuples) have structural equality semantics. This means that they are 
    //compared by comparing the actual value stored in them and not by comparing references (even if they are reference types)
    //All standard F# types are reference types. You can define value type by adding [<Struct>] to your object type declaration, 
    //but discriminated unions will always be reference types
    Id: VertexId
    Neighbours: Vertex<'a> list
    Weight: 'a option 
  }  

  type Graph<'a> = {
    VerticesCount: int
    EdgesCount: int
    Vertices: Vertex<'a> array
  }
  
  /// Parse a file and returns a Graph
  /// File format:
  ///
  /// Line 1: n (#vertices) m (# edges)
  ///         vertices use 1 based indices, 1 to n
  /// Next m lines = edge u v with id (>= 1 and <= n) - directed or undirected according to the problem.
  ///              | edge u v w - includes the weight 
  /// Graph should be simple - no self loops nor parallel edges.
  ///
  /// ## Parameters
  ///  - `file` - full (relative or absolute) string path to the file to open
  let readGraph file = 42

  // open System;;
  // Environment.CurrentDirectory;;
  // Environment.CurrentDirectory <- @"c:\temp";;
  //
  // Environment.GetEnvironmentVariable("PATH")
  //
  // open System.IO;;
  // Directory.GetFiles(".");;
  //
  // File.ReadLines("x.y") |> seq.iter ... // iter 
  //
  // File.ReadAllText(fname, [encoding])
  //
  // File.Exists("x.y")
  // Directory.Exists("x.y")
  //
  let rec getAllFiles dir pattern =
    seq { yield! Directory.EnumerateFiles(dir, pattern)
          for d in Directory.EnumerateDirectories(dir) do
              yield! getAllFiles d pattern }



// Prints the methods, properties, and fields of a type to the console
open System.Reflection
let describeType (ty : Type) =
     
    let bindingFlags = 
        BindingFlags.Public   ||| BindingFlags.NonPublic |||
        BindingFlags.Instance ||| BindingFlags.Static    |||
        BindingFlags.DeclaredOnly
         
    let methods = 
        ty.GetMethods(bindingFlags) 
        |> Array.fold (fun desc meth -> desc + sprintf "%s\r\n" meth.Name) ""
            
    let props = 
        ty.GetProperties(bindingFlags)
        |> Array.fold (fun desc prop -> desc + sprintf "%s\r\n" prop.Name) ""
     
    let fields =
        ty.GetFields(bindingFlags)
        |> Array.fold (fun desc field -> desc + sprintf "%s\r\n" field.Name) ""
 
    printfn "Name: %s" ty.Name
    printfn "Methods:    \n\t%s\n" methods
    printfn "Properties: \n\t%s\n" props
    printfn "Fields:     \n\t%s" fields