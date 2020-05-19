/// Contains the implementation of the state passed around in algorithm implementation.
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module NikonTheThird.SortingAlgorithms.AlgorithmState

open NikonTheThird.SortingAlgorithms.Algorithm
open NikonTheThird.SortingAlgorithms.AsyncState
open NikonTheThird.SortingAlgorithms.Helpers
open System
open System.Windows
open System.Windows.Controls
open System.Windows.Media
open System.Windows.Media.Animation
open System.Windows.Shapes


/// The color and opacity an item in the main array is displayed as.
let primaryArrayColor = struct (Colors.Black, 1.0)


/// The color and opacity an item in the working array is displayed as.
let secondaryArrayColor = struct (Colors.Red, 0.75)


type AlgorithmIndex with
    /// Returns the array index (the first array of the Items).
    member this.ArrayIndex =
        match this with
        | Primary _ -> 0
        | Secondary _ -> 1

    /// Returns the raw index (the second array of the Items).
    member this.Index =
        match this with
        | Primary index | Secondary index -> index

    /// Returns the color and opacity for ellipses with the given index.
    member this.Color =
        match this with
        | Primary _ -> primaryArrayColor
        | Secondary _ -> secondaryArrayColor

    /// Returns the color of the new index if the indexes are different arrays.
    member this.GetTransferColor newIndex =
        match struct (this, newIndex) with
        | struct (Primary _, Primary _) | struct (Secondary _ , Secondary _) -> ValueNone
        | _ -> ValueSome newIndex.Color


/// A sortable item of the algorithm.
type AlgorithmItem = {
    /// The original layout index of the item.
    /// This is used to correctly place items into a layout, for example when
    /// resetting the alternating layout.
    Index : int32
    /// The proper integer value of the item. This determines its spot.
    Value : int32
    /// The ellipse object representing the item.
    Ellipse : Ellipse
    /// The label underneath the ellipse, if it exists.
    Label : TextBlock voption
}


/// Implements the IAlgorithmState interface and is set as the hidden state of the algorithm.
type AlgorithmState = {
    /// Returns the currently set delay value if called. This accesses a WPF element, so it must
    /// be called through a Dispatcher.
    Delay : unit -> int32
    /// The items to sort. The first index indicates the primary or secondary array,
    /// The second index points to an actual item inside one of the arrays.
    Items : AlgorithmItem voption[][]
    /// The line used to indicate a comparison on screen.
    ComparisonMarker : Line
    /// The line used to indicate a swap on screen.
    SwapMarker : Line
    /// The number of performed comparisons.
    ComparisonCount : int32
    /// The text block for the comparison count.
    ComparisonCountLabel : TextBlock
    /// The number of performed swaps.
    SwapCount : int32
    /// The text block for the swap count.
    SwapCountLabel : TextBlock
    /// When the algorithm has been paused, returns a task to be awaited.
    /// Otherwise returns nothing, in this case any further delays have to
    /// be respected. Must be called through a Dispatcher.
    GetPauseTask : unit -> Async<unit> voption
    /// Pauses the algorithm.
    Pause : Async<unit>
} with

    /// Attempts to fetch an item at the given index.
    /// Returns ValueNone when the index is unoccupied.
    member private this.TryGetItem index =
        match index with
        | Primary index -> this.Items.[0].[index]
        | Secondary index -> this.Items.[1].[index]

    /// Fetches an item at the given index.
    /// Throws an exception when the index is unoccupied.
    member private this.GetItem index =
        this.TryGetItem index
        |> ValueOption.defaultWith (fun () -> failwithf "An item does not exist at %A." index)

    /// Handles an ellipse and its label, with optional animations.
    /// Calls to this method have to be dispatched to the main thread.
    member private this.HandleEllipse (item, location, colorAndOpacity, showAnimation) =
        let delay = this.Delay ()
        let animationDuration = float delay * 0.7 |> TimeSpan.FromMilliseconds |> Duration
        let currentLocation = Canvas.GetLeft item.Ellipse
        // Handle the ellipse.
        if showAnimation then
            item.Ellipse.BeginAnimation (
                Canvas.LeftProperty,
                DoubleAnimation (
                    Duration = animationDuration,
                    AccelerationRatio = 0.2,
                    DecelerationRatio = 0.2,
                    FillBehavior = FillBehavior.Stop,
                    From = Nullable currentLocation,
                    To = Nullable location
                )
            )
        Canvas.SetLeft (item.Ellipse, location)
        // Handle the label.
        item.Label |> ValueOption.iter (fun label ->
            if label.Visibility = Visibility.Visible then
                let getLabelLeft value = value + item.Ellipse.ActualWidth / 2.0 - label.CalculatedTextSize.Width / 2.0
                if showAnimation then
                    label.BeginAnimation (
                        Canvas.LeftProperty,
                        DoubleAnimation (
                            Duration = animationDuration,
                            AccelerationRatio = 0.2,
                            DecelerationRatio = 0.2,
                            FillBehavior = FillBehavior.Stop,
                            From = Nullable (getLabelLeft currentLocation),
                            To = Nullable (getLabelLeft location)
                        )
                    )
                Canvas.SetLeft (label, getLabelLeft location)
        )
        // Handle the color and opacity.
        colorAndOpacity |> ValueOption.iter (fun struct (color, opacity) ->
            let brush = item.Ellipse.Fill :?> SolidColorBrush
            brush.BeginAnimation (
                SolidColorBrush.ColorProperty,
                ColorAnimation (
                    Duration = animationDuration,
                    FillBehavior = FillBehavior.Stop,
                    From = Nullable brush.Color,
                    To = Nullable color
                )
            )
            brush.Color <- color
            brush.BeginAnimation (
                SolidColorBrush.OpacityProperty,
                DoubleAnimation (
                    Duration = animationDuration,
                    FillBehavior = FillBehavior.Stop,
                    From = Nullable brush.Opacity,
                    To = Nullable opacity
                )
            )
            brush.Opacity <- opacity
        )

    /// Handles an ellipse marker, with optional animations.
    /// Calls to this method have to be dispatched to the main thread.
    member private this.HandleMarker (marker : Line, item1, item2, animateTo) = 
        let delay = this.Delay ()
        let animationDuration = float delay * 0.7 |> TimeSpan.FromMilliseconds |> Duration
        let struct (x2, y2) =
            match item2 with
            | Choice1Of2 item ->
                struct (
                    Canvas.GetLeft item.Ellipse + item.Ellipse.ActualWidth / 2.0,
                    Canvas.GetTop item.Ellipse + item.Ellipse.ActualHeight / 2.0
                )
            | Choice2Of2 location ->
                struct (
                    location + item1.Ellipse.ActualWidth / 2.0,
                    Canvas.GetTop item1.Ellipse + item1.Ellipse.ActualHeight / 2.0
                )
        // Place the marker at the correct location.
        marker.X1 <- Canvas.GetLeft item1.Ellipse + item1.Ellipse.ActualWidth / 2.0
        marker.Y1 <- Canvas.GetTop item1.Ellipse + item1.Ellipse.ActualHeight / 2.0
        marker.X2 <- x2
        marker.Y2 <- y2
        marker.Visibility <- Visibility.Visible
        // Animate the marker if necessary.
        animateTo |> ValueOption.iter (fun struct (x1, x2) ->
            marker.BeginAnimation (
                Line.X1Property,
                DoubleAnimation (
                    Duration = animationDuration,
                    AccelerationRatio = 0.2,
                    DecelerationRatio = 0.2,
                    FillBehavior = FillBehavior.Stop,
                    From = Nullable marker.X1,
                    To = Nullable (x1 + item1.Ellipse.ActualWidth / 2.0)
                )
            )
            marker.X1 <- x1 + item1.Ellipse.ActualWidth / 2.0
            marker.BeginAnimation (
                Line.X2Property,
                DoubleAnimation (
                    Duration = animationDuration,
                    AccelerationRatio = 0.2,
                    DecelerationRatio = 0.2,
                    FillBehavior = FillBehavior.Stop,
                    From = Nullable marker.X2,
                    To = Nullable (x2 + item1.Ellipse.ActualWidth / 2.0)
                )
            )
            marker.X2 <- x2 + item1.Ellipse.ActualWidth / 2.0
        )

    /// Shows on the screen that a comparison has been performed.
    member private this.VisualCompare (index1, index2) = async {
        let item1 = this.GetItem index1
        let item2 = this.GetItem index2
        let pauseAndDelayOperation = item1.Ellipse.Dispatcher.InvokeAsync (fun () ->
            let delay = this.Delay ()
            this.SwapMarker.Visibility <- Visibility.Hidden
            if delay < 1 then
                // When there is no delay, do not even show a marker.
                this.ComparisonMarker.Visibility <- Visibility.Hidden
            else
                // Otherwise, show the comparison marker (it is static and never animated).
                this.HandleMarker (this.ComparisonMarker, item1, Choice1Of2 item2, ValueNone)
            // Update the comparison count.
            this.ComparisonCountLabel.Text <- sprintf "C %06d" (this.ComparisonCount + 1)
            struct (delay, this.GetPauseTask ())
        )
        let! struct (delay, pauseTask) = Async.AwaitTask pauseAndDelayOperation.Task
        match pauseTask with
        | ValueSome pauseTask -> do! pauseTask
        | ValueNone -> do! Async.Sleep delay
    }

    /// Shows on the screen that a swap has been performed.
    member private this.VisualSwap (index1, index2) = async {
        let item1 = this.GetItem index1
        let item2 = this.TryGetItem index2
        let pauseAndDelayOperation = item1.Ellipse.Dispatcher.InvokeAsync (fun () ->
            let delay = this.Delay ()
            this.ComparisonMarker.Visibility <- Visibility.Hidden
            if delay < 1 then
                // When there is no delay, do not even show a marker.
                // Swap the ellipses without animation.
                this.SwapMarker.Visibility <- Visibility.Hidden
                match item2 with
                | ValueSome item2 ->
                    let newItem1Location, newItem2Location = Canvas.GetLeft item2.Ellipse, Canvas.GetLeft item1.Ellipse
                    this.HandleEllipse (item1, newItem1Location, index1.GetTransferColor index2, false)
                    this.HandleEllipse (item2, newItem2Location, index2.GetTransferColor index1, false)
                | ValueNone ->
                    let newItem1Location = double index2.Index * item1.Ellipse.ActualWidth
                    this.HandleEllipse (item1, newItem1Location, index1.GetTransferColor index2, false)
            elif delay < 50 then
                // When there is very little delay, show the marker but no animations.
                match item2 with
                | ValueSome item2 ->
                    let newItem1Location, newItem2Location = Canvas.GetLeft item2.Ellipse, Canvas.GetLeft item1.Ellipse
                    this.HandleEllipse (item1, newItem1Location, index1.GetTransferColor index2, false)
                    this.HandleEllipse (item2, newItem2Location, index2.GetTransferColor index1, false)
                    this.HandleMarker (this.SwapMarker, item1, Choice1Of2 item2, ValueNone)
                | ValueNone ->
                    let newItem1Location = double index2.Index * item1.Ellipse.ActualWidth
                    this.HandleMarker (this.SwapMarker, item1, Choice2Of2 newItem1Location, ValueNone)
                    this.HandleEllipse (item1, newItem1Location, index1.GetTransferColor index2, false)
            else
                // When there is enough delay, show the marker and the animations.
                match item2 with
                | ValueSome item2 ->
                    let newItem1Location, newItem2Location = Canvas.GetLeft item2.Ellipse, Canvas.GetLeft item1.Ellipse
                    this.HandleMarker (this.SwapMarker, item1, Choice1Of2 item2, ValueSome struct (newItem1Location, newItem2Location))
                    this.HandleEllipse (item1, newItem1Location, index1.GetTransferColor index2, true)
                    this.HandleEllipse (item2, newItem2Location, index2.GetTransferColor index1, true)
                | ValueNone ->
                    let newItem1Location = double index2.Index * item1.Ellipse.ActualWidth
                    this.HandleMarker (this.SwapMarker, item1, Choice1Of2 item1, ValueSome struct (Canvas.GetLeft item1.Ellipse, newItem1Location))
                    this.HandleEllipse (item1, newItem1Location, index1.GetTransferColor index2, true)
            // Update the swap count.
            this.SwapCountLabel.Text <- sprintf "S %06d" (this.SwapCount + 1)
            struct (delay, this.GetPauseTask ())
        )
        let! struct (delay, pauseTask) = Async.AwaitTask pauseAndDelayOperation.Task
        match pauseTask with
        | ValueSome pauseTask -> do! pauseTask
        | ValueNone -> do! Async.Sleep delay
    }

    interface IAlgorithmState with

        /// Returns the number of items to sort.
        member this.Count = async {
            // Return the length of the primary array.
            return this.Items.[0].Length
        }

        /// Performs a comparison of the given indexes, returning the ordering.
        member this.Compare (index1, index2) = async {
            let item1, item2 = this.GetItem index1, this.GetItem index2
            do! this.VisualCompare (index1, index2)
            let comparisonResult =
                match item1.Value.CompareTo item2.Value with
                | value when value < 0 -> LessThan
                | value when value > 0 -> GreaterThan
                | _ -> Equal
            return struct (
                { this with ComparisonCount = this.ComparisonCount + 1 } :> _,
                comparisonResult
            )
        }

        /// Swaps the items at the given indexes.
        member this.Swap (index1, index2) = async {
            if index1 = index2 then return this :> _ else
            do! this.VisualSwap (index1, index2)
            let items = this.Items |> Array.map Array.copy
            let temp = items.[index1.ArrayIndex].[index1.Index]
            do items.[index1.ArrayIndex].[index1.Index] <- items.[index2.ArrayIndex].[index2.Index]
            do items.[index2.ArrayIndex].[index2.Index] <- temp
            return { this with Items = items; SwapCount = this.SwapCount + 1 } :> _
        }

        /// Shows a text label for the given index. If the string is null or empty,
        /// the text label will be removed. Invalid indexes have no effect and return a label
        /// that does nothing.
        member this.Label (index : AlgorithmIndex, text) = async {
            let item =
                this.Items
                |> Array.tryItem index.ArrayIndex
                |> Option.bind (Array.tryItem index.Index)
                |> Option.bind ValueOption.toOption
            /// Updates the label to the given text. Also manages visibility and location.
            let updateLabel text =
                item
                |> Option.iter (fun item ->
                    item.Ellipse.Dispatcher.BeginInvoke (Action (fun () ->
                        item.Label |> ValueOption.iter (fun label ->
                            if String.IsNullOrWhiteSpace text then
                                label.Visibility <- Visibility.Hidden
                            else
                                label.Visibility <- Visibility.Visible
                                label.Text <- text.Trim ()
                                Canvas.SetLeft (label, Canvas.GetLeft item.Ellipse + item.Ellipse.ActualWidth / 2.0 - label.CalculatedTextSize.Width / 2.0)
                        )
                    ))
                    |> ignore
                )
            do updateLabel text
            return {
                new IAlgorithmLabel with
                    member _.Update text = async {
                        do updateLabel text
                    }

                interface IDisposable with
                    member _.Dispose () =
                        updateLabel null
            }
        }

        /// Pauses the algorithm.
        member this.Pause = async {
            return! this.Pause
        }


/// Executes the given algorithm against the given state.
/// The cancellation token can be used to interrupt the execution.
/// The executionFinished function is called upon error or completion.
[<CompiledName "RunAlgorithm">]
let runAlgorithm (algorithm : Algorithm) (state : AlgorithmState) token executionFinished = Async.Start (async {
    let! result = AsyncState.RunIgnoreStateAsync (algorithm, state :> _) |> Async.Catch
    // Find any item so we can access the dispatcher.
    let item = state.Items |> Seq.collect id |> Seq.pick ValueOption.toOption
    let operation = item.Ellipse.Dispatcher.InvokeAsync (fun () ->
        state.ComparisonMarker.Visibility <- Visibility.Hidden
        state.SwapMarker.Visibility <- Visibility.Hidden
        // Hide all the labels.
        state.Items
        |> Seq.collect id
        |> Seq.iter (
            ValueOption.bind (fun item -> item.Label)
            >> ValueOption.iter (fun label -> label.Visibility <- Visibility.Hidden)
        )
        match result with
        | Choice1Of2 () -> executionFinished ValueNone
        | Choice2Of2 ex -> executionFinished (ValueSome ex)
    )
    do! Async.AwaitTask operation.Task
}, token)
