/// Contains the cocktail shaker sort implementation.
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module NikonTheThird.SortingAlgorithms.Algorithms.CocktailShakerSort

open NikonTheThird.SortingAlgorithms.Algorithm
open NikonTheThird.SortingAlgorithms.Helpers


/// https://en.wikipedia.org/wiki/Cocktail_shaker_sort
[<Algorithm "Cocktail Shaker Sort">]
let cocktailShakerSort : Algorithm = algorithm {
    let! count = getCount
    let mutable minLabel = ValueNone
    let mutable maxLabel = ValueNone

    /// The lowest index to compare.
    /// Everything before this index is already sorted.
    let mutable minIndex = 0

    /// The highest index to compare.
    /// Everything after this index is already sorted.
    let mutable maxIndex = count - 2

    // Keep going until the unsorted portion becomes empty.
    while minIndex <= maxIndex do
        // Tracks the last index where we performed a swap going from top to bottom.
        let mutable newMinIndex = maxIndex

        // Tracks the last index where we performed a swap going from bottom to top.
        let mutable newMaxIndex = minIndex

        // Bubble sort left to right.
        for index = minIndex to maxIndex do
            match! compare index (index + 1) with
            | GreaterThan ->
                do! swap index (index + 1)
                do newMaxIndex <- index
            | _ -> do ()

        // Subtract 1 because the item at newMaxIndex is already in the right spot.
        do maxIndex <- newMaxIndex - 1

        // Mark the largest placed element with a label.
        do ValueOption.dispose maxLabel
        let! maxLabel' = setLabel' (Primary (newMaxIndex + 1)) "R"
        do maxLabel <- ValueSome maxLabel'

        // Bubble sort right to left.
        for index = maxIndex downto minIndex do
            match! compare index (index + 1) with
            | GreaterThan ->
                do! swap index (index + 1)
                do newMinIndex <- index
            | _ -> do ()

        // Add 1 because the item at newMinIndex is already in the right spot.
        do minIndex <- newMinIndex + 1

        // Mark the smallest placed element with a label.
        do ValueOption.dispose minLabel
        let! minLabel' = setLabel' (Primary newMinIndex) "L"
        do minLabel <- ValueSome minLabel'
}
