/// Contains helper functions for the sorting algorithm visualizer.
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module NikonTheThird.SortingAlgorithms.Helpers

open System.Globalization
open System.Windows
open System.Windows.Controls
open System.Windows.Media


// Contains extensions to the WPF TextBlock.
type TextBlock with
    
    /// Returns the size of the Text property even when the TextBlock size
    /// has not been computed yet.
    member this.CalculatedTextSize =
        let formattedText =
            FormattedText (
                this.Text,
                CultureInfo.CurrentCulture,
                FlowDirection.LeftToRight,
                Typeface (this.FontFamily, this.FontStyle, this.FontWeight, this.FontStretch),
                this.FontSize,
                Brushes.Black,
                1.0
            )
        {| Width = formattedText.Width; Height = formattedText.Height |}


/// Contains additional functions for value options.
[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module ValueOption =

    /// Converts the given value option to a regular option.
    [<CompiledName "ToOption">]
    let toOption = function
    | ValueSome value -> Some value
    | ValueNone -> None

    /// Disposes an optional disposable inside the value option.
    [<CompiledName "Dispose">]
    let dispose disposable =
        disposable
        |> ValueOption.iter (using >> (|>) ignore)
