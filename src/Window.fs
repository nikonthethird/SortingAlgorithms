/// Contains the logic for the WPF window.
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module NikonTheThird.SortingAlgorithms.Window

open FSharp.Reflection
open FsXaml
open NikonTheThird.SortingAlgorithms.Algorithm
open NikonTheThird.SortingAlgorithms.AlgorithmState
open NikonTheThird.SortingAlgorithms.Helpers
open System
open System.Globalization
open System.Reflection
open System.Threading
open System.Windows
open System.Windows.Controls
open System.Windows.Input
open System.Windows.Media
open System.Windows.Shapes


/// The sortable items counts selectable in the context menu of the window.
let private itemCounts = [10; 25; 50; 75; 100; 150; 200; 300; 600]


/// The various ways the items can be arranged before sorting.
type private ItemLayouts = Randomized | Ascending | Descending | Alternating | Arbitrary | Crossed


/// The swap and comparison delays in milliseconds selectable in the context menu of the window.
let private delays = [0; 1; 5; 10; 15; 20; 25; 50; 75; 100; 175; 250; 500; 1000; 2000]


/// The name of the algorithm checked by default.
let [<Literal>] private DefaultAlgorithm = "Bubble Sort"


/// The sortable item count checked by default.
let [<Literal>] private DefaultItemCount = 25


/// The item arrangement checked by default.
let private defaultItemLayout = Randomized


/// The swap and comparison delay in milliseconds checked by default.
let [<Literal>] private DefaultDelay = 175


// The main window.
type AlgorithmWindow = XAML<"Window.xaml">


/// Contains information about an algorithm detected by looking for the AlgorithmAttribute.
type private DetectedAlgorithm = {
    /// The display name of the algorithm.
    Name : string
    /// The implementation of the algorithm.
    Algorithm : Algorithm
}


/// Constructs new sortable items and places the new algorithm state into the tag of the item canvas.
let private createItems (window : AlgorithmWindow) itemCount itemLayout =
    let itemCount =
        itemCount
        |> ValueOption.defaultWith (fun () ->
            window.ItemCount.Items
            |> Seq.cast<MenuItem>
            |> Seq.tryPick (fun item -> if item.IsChecked then Some (unbox item.Tag) else None)
            |> Option.defaultValue DefaultItemCount
        )
    let itemLayout =
        itemLayout
        |> ValueOption.defaultWith (fun () ->
            window.ItemLayout.Items
            |> Seq.cast<MenuItem>
            |> Seq.tryPick (fun item -> if item.IsChecked then Some (unbox item.Tag) else None)
            |> Option.defaultValue defaultItemLayout
        )
    // Remove all current elements on the canvas.
    window.ItemCanvas.Children.Clear ()
    /// A line used to visually indicate that a comparison is taking place.
    let comparisonMarker =
        Line (
            Stroke = SolidColorBrush (Color.FromArgb (0x4fuy, 0x00uy, 0xccuy, 0x00uy)),
            StrokeStartLineCap = PenLineCap.Round,
            StrokeEndLineCap = PenLineCap.Round,
            StrokeThickness = max window.ItemCanvas.ActualHeight window.ItemCanvas.ActualWidth / float itemCount * 2.0,
            Visibility = Visibility.Hidden
        )
    window.ItemCanvas.Children.Add comparisonMarker |> ignore
    /// A line used to visually indicate that a swap is taking place.
    let swapMarker =
        Line (
            Stroke = SolidColorBrush (Color.FromArgb (0x4fuy, 0x00uy, 0x00uy, 0xccuy)),
            StrokeStartLineCap = PenLineCap.Round,
            StrokeEndLineCap = PenLineCap.Round,
            StrokeThickness = max window.ItemCanvas.ActualHeight window.ItemCanvas.ActualWidth / float itemCount * 2.0,
            Visibility = Visibility.Hidden
        )
    window.ItemCanvas.Children.Add swapMarker |> ignore
    let random = Random ()
    /// Array containing all sortable items.
    let algorithmItems = Array.init itemCount (fun index ->
        let value =
            match itemLayout with
            | Randomized | Ascending | Descending | Crossed -> index
            | Alternating -> if index % 2 = 0 then 0 else itemCount - 1
            | Arbitrary -> random.Next itemCount
        /// An ellipse visually representing the item.
        let ellipse =
            Ellipse (
                Fill = (let struct (color, _) = primaryArrayColor in SolidColorBrush color),
                Height = window.ItemCanvas.ActualHeight / float itemCount,
                Width = window.ItemCanvas.ActualWidth / float itemCount
            )
        window.ItemCanvas.Children.Add ellipse |> ignore
        Canvas.SetTop (ellipse, float (itemCount - value - 1) * ellipse.Height)
        Canvas.SetLeft (ellipse, float index * ellipse.Width)
        /// An optional text label for the item.
        let label =
            if itemCount > 100 then ValueNone else
            let fontSize = 16.0 / (max 0.0 (double itemCount - 25.0) / 75.0 + 1.0)
            let label = 
                TextBlock (
                    Visibility = Visibility.Hidden,
                    FontSize = fontSize,
                    Text = string value
                )
            window.ItemCanvas.Children.Add label |> ignore
            Canvas.SetTop (label, Canvas.GetTop ellipse + ellipse.Height)
            Canvas.SetLeft (label, Canvas.GetLeft ellipse + ellipse.Width / 2.0 - label.CalculatedTextSize.Width / 2.0)
            ValueSome label
        ValueSome {
            Index = index
            Value = value
            Ellipse = ellipse
            Label = label
        }
    )
    /// Returns the current delay for comparisons and swaps.
    let delay () =
        window.Delay.Tag
        |> Option.ofObj
        |> Option.map unbox
        |> Option.defaultValue DefaultDelay
    /// If the algorithm is paused, returns a task for it to await.
    let getPauseTask () =
        let pauseEvent = window.Pause.Tag :?> ManualResetEventSlim
        if pauseEvent.IsSet
        then ValueNone
        else ValueSome (pauseEvent.WaitHandle |> Async.AwaitWaitHandle |> Async.Ignore)
    /// Pauses the algorithm. This can be used from inside an algorithm.
    let pause = async {
        let operation = window.Pause.Dispatcher.InvokeAsync (fun () ->
            let pauseEvent = window.Pause.Tag :?> ManualResetEventSlim
            if pauseEvent.IsSet then
                pauseEvent.Reset ()
                window.Pause.IsChecked <- true
                window.ItemBorder.Background <- Brushes.LightGray
        )
        do! Async.AwaitTask operation.Task
    }
    // Place the new state into the canvas tag.
    window.ItemCanvas.Tag <- {
        Delay = delay
        Items = [| algorithmItems; Array.replicate itemCount ValueNone |]
        ComparisonMarker = comparisonMarker
        SwapMarker = swapMarker
        ComparisonCount = 0
        ComparisonCountLabel = window.StatusCompares
        SwapCount = 0
        SwapCountLabel = window.StatusSwaps
        GetPauseTask = getPauseTask
        Pause = pause
    }


/// Handler called when another item count has been selected.
let private handleItemCountSelected window itemCount =
    createItems window (ValueSome itemCount) ValueNone


/// Handler called when another item layout has been selected.
let private handleItemLayoutSelected window itemLayout =
    createItems window ValueNone (ValueSome itemLayout)


/// Distribute the items to sort on the screen and hide all labels.
/// The items are distributed according to the layout.
let private scrambleItems (window : AlgorithmWindow) =
    let state = unbox window.ItemCanvas.Tag  
    let indexPermutation = [|0 .. state.Items.[0].Length - 1|]
    let itemLayout =
        window.ItemLayout.Items
        |> Seq.cast<MenuItem>
        |> Seq.tryPick (fun item -> if item.IsChecked then Some (unbox item.Tag) else None)
        |> Option.defaultValue defaultItemLayout
    match itemLayout with
    | Randomized ->
        let random = Random ()
        for index = 0 to indexPermutation.Length - 1 do
            let newIndex = random.Next (index, indexPermutation.Length)
            let temp = indexPermutation.[index]
            indexPermutation.[index] <- indexPermutation.[newIndex]
            indexPermutation.[newIndex] <- temp
    | Ascending | Alternating | Arbitrary ->
        // Keep the identity permutation.
        ()
    | Descending ->
        Array.Reverse indexPermutation
    | Crossed ->
        for index in 0 .. 2 .. indexPermutation.Length / 2 - 1 do
            let temp = indexPermutation.[index]
            indexPermutation.[index] <- indexPermutation.[indexPermutation.Length - index - 1]
            indexPermutation.[indexPermutation.Length - index - 1] <- temp
    let scrambledItems =
        state.Items
        |> Seq.collect id
        |> Seq.choose ValueOption.toOption
        |> Seq.sortBy (fun item -> item.Index)
        |> Seq.permute (Array.get indexPermutation)
        |> Seq.mapi (fun index item ->
            Canvas.SetLeft (item.Ellipse, float index * item.Ellipse.ActualWidth)
            item.Ellipse.Fill <- let struct (color, _) = primaryArrayColor in SolidColorBrush color
            item.Label
            |> ValueOption.iter (fun label -> label.Visibility <- Visibility.Hidden)
            ValueSome item
        )
        |> Seq.toArray
    window.ItemCanvas.Tag <- {
        state with Items = [| scrambledItems; Array.replicate state.Items.[0].Length ValueNone |]
    }


/// Adds a handler function to the given menu item's click event.
let private addClickHandler<'T> handler (menu : MenuItem) =
    menu.Click
    |> Event.add (fun e ->
        e.Handled <- true
        menu.IsChecked <- true
        (menu.Parent :?> MenuItem).Items
        |> Seq.cast<MenuItem>
        |> Seq.iter (fun other ->
            if not (Object.ReferenceEquals (menu, other)) then
                other.IsChecked <- false
        )
        handler (unbox<'T> menu.Tag)
    )
    menu


/// Populates the algorithm context menu.
let private createAlgorithmMenu (window : AlgorithmWindow) =
    typeof<AlgorithmWindow>.Assembly.GetTypes ()
    |> Seq.filter FSharpType.IsModule
    |> Seq.collect (fun modType -> modType.GetProperties (BindingFlags.Public ||| BindingFlags.Static))
    |> Seq.choose (fun modProp ->
        modProp.GetCustomAttribute<AlgorithmAttribute> false
        |> Option.ofObj
        |> Option.map (fun algorithmAttribute -> {
            Name = algorithmAttribute.Name
            Algorithm = modProp.GetValue null |> unbox
        })
    )
    |> Seq.sortBy (fun { Name = name } -> name)
    |> Seq.iter (fun detectedAlgorithm ->
        MenuItem (
            Header = detectedAlgorithm.Name,
            Tag = detectedAlgorithm,
            IsChecked = (detectedAlgorithm.Name = DefaultAlgorithm)
        )
        |> addClickHandler ignore
        |> window.Algorithm.Items.Add
        |> ignore
    )
    window


/// Populates the item count context menu.
let private createItemCountMenu (window : AlgorithmWindow) =
    itemCounts
    |> List.iter (fun itemCount ->
        MenuItem (
            Header = sprintf "%d Items" itemCount,
            Tag = itemCount,
            IsChecked = (itemCount = DefaultItemCount)
        )
        |> addClickHandler (handleItemCountSelected window)
        |> window.ItemCount.Items.Add
        |> ignore
    )
    window


/// Populate the item layout menu.
let private createItemLayoutMenu (window : AlgorithmWindow) =
    FSharpType.GetUnionCases (typeof<ItemLayouts>, true)
    |> Array.iter (fun itemLayoutCase ->
        let itemLayout = FSharpValue.MakeUnion (itemLayoutCase, Array.empty, true) |> unbox
        MenuItem (
            Header = itemLayoutCase.Name,
            Tag = itemLayout,
            IsChecked = (itemLayout = defaultItemLayout)
        )
        |> addClickHandler (handleItemLayoutSelected window)
        |> window.ItemLayout.Items.Add
        |> ignore
    )
    window


/// Populate the delay context menu.
let private createDelayMenu (window : AlgorithmWindow) =
    delays
    |> List.iter (fun delay ->
        let header =
            if delay = 0 then "None" else
            if delay < 1000 then sprintf "%d ms" delay else
            sprintf "%d s" (delay / 1000)
        MenuItem (
            Header = header,
            Tag = delay,
            IsChecked = (delay = DefaultDelay)
        )
        |> addClickHandler (fun delay -> window.Delay.Tag <- box<int32> delay)
        |> window.Delay.Items.Add
        |> ignore
    )
    window


/// Creates a handler for the start / stop button.
let private createStartStopButton (window : AlgorithmWindow) =
    window.StartStop.Click
    |> Event.add (fun _ ->
        window.Algorithm.Items
        |> Seq.cast<MenuItem>
        |> Seq.tryPick (fun item ->
            if item.IsChecked
            then Some (unbox<DetectedAlgorithm> item.Tag)
            else None
        )
        |> Option.iter (fun { Algorithm = algorithm; Name = name } ->
            let finishExecution error =
                window.StartStop.Tag <- null
                window.StartStop.Header <- "Start"
                window.Algorithm.IsEnabled <- true
                window.ItemCount.IsEnabled <- true
                window.ItemLayout.IsEnabled <- true
                window.Pause.IsEnabled <- false
                window.Pause.IsChecked <- false
                window.ItemBorder.Background <- null
                let pauseEvent = window.Pause.Tag :?> ManualResetEventSlim
                pauseEvent.Set ()
                error |> ValueOption.iter (fun (ex : exn) ->
                    MessageBox.Show (window, sprintf "Algorithm Error:\n\n%O" ex, window.Title, MessageBoxButton.OK, MessageBoxImage.Error) |> ignore
                )
            match window.StartStop.Tag with
            | :? CancellationTokenSource as cts ->
                cts.Cancel ()
                finishExecution ValueNone
            | _ ->
                let cts = new CancellationTokenSource ()
                window.StartStop.Tag <- cts
                window.StartStop.Header <- "Stop"
                window.Algorithm.IsEnabled <- false
                window.ItemCount.IsEnabled <- false
                window.ItemLayout.IsEnabled <- false
                window.Pause.IsEnabled <- true
                window.StatusAlgorithm.Text <- name
                window.StatusCompares.Text <- sprintf "C %06d" 0
                window.StatusSwaps.Text <- sprintf "S %06d" 0
                scrambleItems window
                let state = unbox window.ItemCanvas.Tag
                runAlgorithm algorithm state cts.Token finishExecution
        )
    )
    window


/// Creates a handler for the pause button.
let private createPauseButton (window : AlgorithmWindow) =
    window.Pause.Tag <- new ManualResetEventSlim true
    window.Pause.Click
    |> Event.add (fun _ ->
        let pauseEvent = window.Pause.Tag :?> ManualResetEventSlim
        if pauseEvent.IsSet then
            // The algorithm is active and can be paused.
            pauseEvent.Reset ()
            window.Pause.IsChecked <- true
            window.ItemBorder.Background <- Brushes.LightGray
        else
            // The algorithm is paused and can be resumed.
            pauseEvent.Set ()
            window.Pause.IsChecked <- false
            window.ItemBorder.Background <- null
    )
    window


/// Handles the zoom level control in the status bar.
let private handleZoomLevel (window : AlgorithmWindow) =
    window.StatusZoom.SelectionChanged
    |> Event.add (fun e ->
        let tag = ((e.AddedItems.[0]) :?> ComboBoxItem).Tag :?> string
        let newZoomLevel = Double.Parse (tag, CultureInfo.InvariantCulture)
        window.CanvasScale.ScaleX <- newZoomLevel
        window.CanvasScale.ScaleY <- newZoomLevel
    )
    window


/// Handles the window's preview key down event, which is used for pausing.
let private handlePreviewKeyDown (window : AlgorithmWindow) =
    window.PreviewKeyDown
    |> Event.add (fun e ->
        if e.Key = Key.Space && window.Pause.IsEnabled then
            e.Handled <- true
            RoutedEventArgs MenuItem.ClickEvent
            |> window.Pause.RaiseEvent
    )
    window


/// Subscribes to the windows' loaded event, where most of the
/// interface is created.
let subscribeToLoadedEvent (window : AlgorithmWindow) =
    window.Loaded
    |> Event.add (fun _ ->
        window
        |> createAlgorithmMenu
        |> createItemCountMenu
        |> createItemLayoutMenu
        |> createDelayMenu
        |> createStartStopButton
        |> createPauseButton
        |> handleZoomLevel
        |> handlePreviewKeyDown
        |> ignore
        createItems window (ValueSome DefaultItemCount) (ValueSome defaultItemLayout)
    )
    window


/// Subscribes to the windows' closing event, where any running
/// algorithm will be stopped.
let subscribeToClosingEvent (window : AlgorithmWindow) =
    window.Closing
    |> Event.add (fun _ ->
        match window.StartStop.Tag with
        | :? CancellationTokenSource as cts ->
            cts.Cancel ()
            window.StartStop.Tag <- null
        | _ -> ()
    )
    window
