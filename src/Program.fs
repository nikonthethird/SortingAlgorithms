/// Contains the main entry point into the application.
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module NikonTheThird.SortingAlgorithms.Program

open NikonTheThird.SortingAlgorithms.Window
open System
open System.Windows


/// The entry point of the application.
[<EntryPoint; STAThread>]
let main _ =
    AlgorithmWindow ()
    |> subscribeToLoadedEvent
    |> subscribeToClosingEvent
    |> (Application ()).Run
