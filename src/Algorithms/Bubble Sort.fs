/// Contains the bubble sort implementation.
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module NikonTheThird.SortingAlgorithms.Algorithms.BubbleSort

open NikonTheThird.SortingAlgorithms.Algorithm
open NikonTheThird.SortingAlgorithms.Helpers


/// https://en.wikipedia.org/wiki/Bubble_sort
[<Algorithm "Bubble Sort">]
let bubbleSort : Algorithm = algorithm {
    let! count = getCount
    let mutable maxLabel = ValueNone

    /// The highest index to compare.
    /// Everything after this index is already sorted.
    let mutable maxIndex = count - 1

    while maxIndex >= 1 do
        /// Tracks the last index where we performed a swap.
        let mutable newMaxIndex = 0

        for index = 1 to maxIndex do
            match! compare (index - 1) index with
            | GreaterThan ->
                // Found two reversed items. Swap them.
                do! swap (index - 1) index
                do newMaxIndex <- index - 1
            | _ -> do ()

        // Set the max index to the last index where we performed a swap.
        // This means that if in a case where some items end up sorted at
        // the back of the array, we skip those the next time.
        do maxIndex <- newMaxIndex

        // Mark the largest placed element with a label.
        do ValueOption.dispose maxLabel
        let! maxLabel' = setLabel' (Primary (maxIndex + 1)) "R"
        do maxLabel <- ValueSome maxLabel'
}
