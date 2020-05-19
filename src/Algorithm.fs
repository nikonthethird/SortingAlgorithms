/// Contains functionality that the sorting algorithms can access.
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module NikonTheThird.SortingAlgorithms.Algorithm

open NikonTheThird.SortingAlgorithms.AsyncState
open System
open System.Diagnostics


/// Attribute that identifies a sorting algorithm.
[<AllowNullLiteral; Sealed; AttributeUsage (AttributeTargets.Property)>]
type AlgorithmAttribute (name : string) =
    inherit Attribute ()
    /// The name of the sorting algorithm.
    member _.Name = name


/// An index into the primary or secondary array.
type AlgorithmIndex =
/// An index into the primary array.
| Primary of Index : int32
/// An index into the secondary array.
| Secondary of Index : int32


/// Specifies the ordering returned by a compare operation.
type AlgorithmOrdering = Equal | LessThan | GreaterThan


/// Represents an active label of an algorithm item.
type IAlgorithmLabel =
    inherit IDisposable
    /// Update the text of the label.
    abstract Update : string -> Async<unit>


/// Hidden state carried by every sorting algorithm.
/// This is used for example to access the elements to sort.
type IAlgorithmState =
    /// Returns the number of items to sort.
    abstract Count : Async<int32>
    /// Performs a comparison of the given indexes, returning the ordering.
    abstract Compare : AlgorithmIndex * AlgorithmIndex -> Async<struct (IAlgorithmState * AlgorithmOrdering)>
    /// Swaps the items at the given indexes.
    abstract Swap : AlgorithmIndex * AlgorithmIndex -> Async<IAlgorithmState>
    /// Shows a text label for the given index. If the string is null or empty,
    /// the text label will be removed. Invalid indexes have no effect and return a label
    /// that does nothing.
    abstract Label : AlgorithmIndex * string -> Async<IAlgorithmLabel>
    /// Pauses the algorithm.
    abstract Pause : Async<unit>


/// Represents an algorithmic operation that returns a result.
type Algorithm<'V> = AsyncState<'V, IAlgorithmState>


/// Represents an algorithmic operation that returns no result.
type Algorithm = Algorithm<unit>


/// Computation expression builder for algorithms.
/// This is just a renamed async state computation expression builder.
[<CompiledName "Algorithm">]
let algorithm = asyncState


/// Algorithmic version of the fixed point combinator.
/// The type annotations are present to improve the
/// reported type when implementing an algorithm.
[<CompiledName "AlgorithmFix"; DebuggerStepThrough>]
let rec fix<'T, 'V> f (a : 'T) : Algorithm<'V> = f (fix f) a


/// Algorithmic operation that returns the number of items to sort.
[<CompiledName "GetCount"; DebuggerStepThrough>]
let getCount : Algorithm<int32> = asyncState {
    let! state = getState<IAlgorithmState>
    return! state.Count
}


/// Algorithmic operation that performs a comparison of the given
/// indexes, returning the ordering.
[<CompiledName "AdvancedCompare"; DebuggerStepThrough>]
let compare' (a : AlgorithmIndex) (b : AlgorithmIndex) : Algorithm<AlgorithmOrdering> = asyncState {
    let! state = getState<IAlgorithmState>
    let! struct (state', comparisonResult) = state.Compare (a, b)
    do! setState state'
    return comparisonResult
}


/// Algorithmic operation that performs a comparison of the given
/// indexes in the primary array, returning the ordering.
[<CompiledName "SimpleCompare"; DebuggerStepThrough>]
let compare (a : int32) (b : int32) : Algorithm<AlgorithmOrdering> = asyncState {
    return! compare' (Primary a) (Primary b)
}


/// Algorithmic operation that performs a comparison of the given
/// indexes in the primary array and returns whether the first
/// element is less than the second element.
[<CompiledName "IsLessThan"; DebuggerStepThrough>]
let isLessThan (a : int32) (b : int32) : Algorithm<bool> = asyncState {
    match! compare a b with
    | LessThan -> return true
    | _ -> return false
}


/// Algorithmic operation that performs a comparison of the given
/// indexes in the primary array and returns whether the first
/// element is greater than the second element.
[<CompiledName "IsGreaterThan"; DebuggerStepThrough>]
let isGreaterThan (a : int32) (b : int32) : Algorithm<bool> = asyncState {
    match! compare a b with
    | GreaterThan -> return true
    | _ -> return false
}


/// Algorithmic operation that swaps the items at the given indexes.
/// This can be used to transfer items between the primary and
/// secondary array.
[<CompiledName "AdvancedSwap"; DebuggerStepThrough>]
let swap' (a : AlgorithmIndex) (b : AlgorithmIndex) : Algorithm = asyncState {
    let! state = getState<IAlgorithmState>
    let! state' = state.Swap (a, b)
    do! setState state'
}


/// Algorithmic operation that swaps the items at the given indexes
/// of the primary array.
[<CompiledName "SimpleSwap"; DebuggerStepThrough>]
let swap (a : int32) (b : int32) : Algorithm = asyncState {
    return! swap' (Primary a) (Primary b)
}


/// Algorithmic operation that shows a text label for the given index.
/// Invalid indexes have no effect and return a label that does nothing.
[<CompiledName "AdvancedSetLabel"; DebuggerStepThrough>]
let setLabel' (index : AlgorithmIndex) (text : string) : Algorithm<IAlgorithmLabel> = asyncState {
    let! state = getState<IAlgorithmState>
    return! state.Label (index, text)
}


/// Algorithmic operation that shows a text label for the given primary index.
/// Invalid indexes have no effect.
[<CompiledName "SimpleSetLabel"; DebuggerStepThrough>]
let setLabel (index : int32) (text : string) : Algorithm = asyncState {
    return! setLabel' (Primary index) text |> AsyncState.Ignore
}


/// Algorithmic operation that pauses the algorithm.
[<CompiledName "Pause"; DebuggerStepThrough>]
let pause : Algorithm = asyncState {
    let! state = getState<IAlgorithmState>
    do! state.Pause
}
