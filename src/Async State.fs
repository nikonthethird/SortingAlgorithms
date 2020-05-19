/// Contains an implementation of a monad that is both asynchronous and contains state.
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module NikonTheThird.SortingAlgorithms.AsyncState

open System
open System.Diagnostics
open System.Runtime.CompilerServices


/// Represents an asynchronous computation carrying hidden state.
[<Struct>]
type AsyncState<'V, 'S> =
    private
    /// Internal representation of an asynchronous computation
    /// expression carrying hidden state.
    | AsyncState of Computation : ('S -> Async<struct ('V * 'S)>)


/// Implementation of the asyncState computation expression.
[<Sealed>]
type AsyncStateBuilder internal () =

    /// Wraps the given value inside an async state computation expression.
    [<DebuggerStepThrough>]
    member _.Return<'V, 'S> (value : 'V) : AsyncState<'V, 'S> =
        AsyncState (fun state -> async {
            return struct (value, state)
        })

    /// Returns another async state computation expression directly.
    [<DebuggerStepThrough>]
    member _.ReturnFrom<'V, 'S> (wrapped : AsyncState<'V, 'S>) : AsyncState<'V, 'S> =
        wrapped

    /// Returns an async computation expression directly.
    [<DebuggerStepThrough>]
    member _.ReturnFrom<'V, 'S> (wrapped : Async<'V>) : AsyncState<'V, 'S> =
        AsyncState (fun state -> async {
            let! value = wrapped
            return struct (value, state)
        })

    /// Returns a default async computation expression that evaluates to the unit type.
    [<DebuggerStepThrough>]
    member this.Zero<'S> () : AsyncState<unit, 'S> =
        this.Return<unit, 'S> ()

    /// Extracts the value from the given async state computation expression and calls
    /// the given function with it. Returns the result of the function.
    [<DebuggerStepThrough>]
    member _.Bind<'V1, 'V2, 'S> (AsyncState wrapped : AsyncState<'V1, 'S>, f : 'V1 -> AsyncState<'V2, 'S>) : AsyncState<'V2, 'S> =
        AsyncState (fun state -> async {
            let! struct (value, state') = wrapped state
            return! let (AsyncState wrapped') = f value in wrapped' state'
        })

    /// Extracts the value from the given async computation expression and calls
    /// the given function with it. Returns the result of the function.
    [<DebuggerStepThrough>]
    member this.Bind<'V1, 'V2, 'S> (wrapped : Async<'V1>, f : 'V1 -> AsyncState<'V2, 'S>) : AsyncState<'V2, 'S> =
        this.Bind<'V1, 'V2, 'S> (this.ReturnFrom wrapped, f)

    /// Combines two async state computation expressions into one.
    /// The state of the first computation expression is passed into the second computation expression.
    [<DebuggerStepThrough>]
    member this.Combine<'V, 'S> (wrapped : AsyncState<unit, 'S>, f : unit -> AsyncState<'V, 'S>) : AsyncState<'V, 'S> =
        this.Bind<unit, 'V, 'S> (wrapped, f)

    /// Iterates over the given sequence, calling the given function with each value and passing the state of the previous
    /// iteration through to the next iteration.
    [<DebuggerStepThrough>]
    member _.For<'V, 'S> (sequence : 'V seq, f : 'V -> AsyncState<unit, 'S>) : AsyncState<unit, 'S> =
        AsyncState (fun state -> async {
            let mutable state = state
            for item in sequence do
                let! struct ((), state') = let (AsyncState wrapped) = f item in wrapped state
                do state <- state'
            return struct ((), state)
        })

    /// Repeats the given body while the given guard returns true. The state of the previous iteration is
    /// passed through to the next iteration.
    [<DebuggerStepThrough>]
    member _.While<'S> (guard : unit -> bool, body : unit -> AsyncState<unit, 'S>) : AsyncState<unit, 'S> =
        AsyncState (fun state -> async {
            let mutable state = state
            while guard () do
                let! struct ((), state') = let (AsyncState wrapped) = body () in wrapped state
                do state <- state'
            return struct ((), state)
        })

    /// Repeats the given body while the given async expression guard returns true. The state of the previous guard
    /// and body calls are passed through to the next iteration.
    [<DebuggerStepThrough>]
    member _.While<'S> (guard : unit -> AsyncState<bool, 'S>, body : unit -> AsyncState<unit, 'S>) =
        AsyncState (fun state -> async {
            let! struct (continueLoop', state') = let (AsyncState wrapped) = guard () in wrapped state
            let mutable state = state
            let mutable continueLoop = continueLoop'
            while continueLoop do
                let! struct ((), state') = let (AsyncState wrapped) = body () in wrapped state
                let! struct (continueLoop', state'') = let (AsyncState wrapped) = guard () in wrapped state'
                do state <- state''
                do continueLoop <- continueLoop'
            return struct ((), state)
        })

    /// Attempts to evaluate the given body async state computation expression. If an exception occurs, the given handler function is
    /// called and its result used instead.
    [<DebuggerStepThrough>]
    member _.TryWith<'V, 'S> (body : unit -> AsyncState<'V, 'S>, handler : exn -> AsyncState<'V, 'S>) : AsyncState<'V, 'S> =
        AsyncState (fun state -> async {
            try return! let (AsyncState wrapped) = body () in wrapped state
            with ex -> return! let (AsyncState wrapped) = handler ex in wrapped state
        })

    /// Attempts to evaluate the given body async state computation expression. After the evaluation finished, regardless if an error
    /// has occurred or not, the given compensation function is executed.
    [<DebuggerStepThrough>]
    member _.TryFinally<'V, 'S> (body : unit -> AsyncState<'V, 'S>, compensation : unit -> unit) =
        AsyncState (fun state -> async {
            try return! let (AsyncState wrapped) = body () in wrapped state
            finally compensation ()
        })

    /// Automatically disposes the given resource after the evaluation of the body has finished.
    [<DebuggerStepThrough>]
    member _.Using<'V, 'S, 'D when 'D :> IDisposable> (resource : 'D, body : 'D -> AsyncState<'V, 'S>) : AsyncState<'V, 'S> =
        AsyncState (fun state -> async {
            use resource = resource
            return! let (AsyncState wrapped) = body resource in wrapped state
        })

    /// Delays the immediate execution of the async state computation expression by representing it as a
    /// that has to be called first.
    /// Calls to Delay are automatically added at the appropriate places when using the computation expression syntax.
    [<DebuggerStepThrough>]
    member _.Delay<'V, 'S> (f : unit -> AsyncState<'V, 'S>) : unit -> AsyncState<'V, 'S> =
        f

    /// Executes a delayed async state computation expression.
    /// A call to Run is automatically added as the outermost call when using the computation expression syntax.
    [<DebuggerStepThrough>]
    member _.Run<'V, 'S> (f : unit -> AsyncState<'V, 'S>) : AsyncState<'V, 'S> =
        f ()


/// Computation expression builder for an asynchronous computation carrying hidden state.
[<CompiledName "AsyncState">]
let asyncState = AsyncStateBuilder ()


/// Retrieves the currently stored state of the async state computation expression.
[<CompiledName "GetState">]
let getState<'S> : AsyncState<'S, 'S> =
    AsyncState (fun state -> async {
        return struct (state, state)
    })


/// Updates the stored state of the async state computation expression.
[<CompiledName "SetState">]
let setState<'S> (state : 'S) : AsyncState<unit, 'S> =
    AsyncState (fun _ -> async {
        return struct ((), state)
    })


/// Contains methods for handling async state computation expressions.
[<AbstractClass; Sealed; Extension>]
type AsyncState private () =

    /// Executes the async state computation expression with the given state.
    /// Returns the result of the computation as well as the final state.
    [<Extension; DebuggerStepThrough>]
    static member RunAsync<'V, 'S> (AsyncState wrapped : AsyncState<'V, 'S>, state : 'S) : Async<struct ('V * 'S)> = async {
        return! wrapped state
    }

    /// Executes the async state computation expression with the given state.
    /// Returns the result of the computation and discards the final state.
    [<Extension; DebuggerStepThrough>]
    static member RunIgnoreStateAsync<'V, 'S> (AsyncState wrapped : AsyncState<'V, 'S>, state : 'S) : Async<'V> = async {
        let! struct (value, _) = wrapped state
        return value
    }

    /// Replaces the value inside the async state computation expression
    /// with a unit value.
    static member Ignore<'V, 'S> (wrapped : AsyncState<'V, 'S>) : AsyncState<unit, 'S> =
        asyncState.Bind (wrapped, ignore >> asyncState.Return)
