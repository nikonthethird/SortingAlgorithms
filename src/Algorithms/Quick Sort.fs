/// Contains the quick sort implementation.
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module NikonTheThird.SortingAlgorithms.Algorithms.QuickSort

open NikonTheThird.SortingAlgorithms.Algorithm


/// Compares the items at indexes a and b, and if a > b, swaps them.
let private compareAndSwap a b : Algorithm = algorithm {
    match! compare a b with
    | GreaterThan -> do! swap a b
    | _ -> do ()
}


/// Calculates the median of three for the given range.
/// Returns the position of the median, which will be the pivot item.
let private median low high : Algorithm<int32> = algorithm {
    let center = (low + high) / 2
    /// Helper function to set a label on all three items under observation.
    let setLabels label : Algorithm = algorithm {
        for index in [low; center; high] do
            do! setLabel index label
    }
    // Display a question mark label to indicate that we do not know the median yet.
    do! setLabels "?"
    // Sort the low, center and high items, leaving the median in the center.
    do! compareAndSwap low high
    do! compareAndSwap low center
    do! compareAndSwap center high
    /// Remove the question mark labels.
    do! setLabels null
    /// The center item is now the median.
    return center
}


/// Scans the range of items below the pivot for items that are too large and
/// need to be swapped into the range of items above the pivot.
let rec private lookForLargeItemBelowPivot low pivot : Algorithm<int32> = algorithm {
    // If we're at the pivot, no need to look further. The pivot is the next item
    // to be considered for swapping.
    if low + 1 = pivot then return pivot else
    match! compare (low + 1) pivot with
    | LessThan -> return! lookForLargeItemBelowPivot (low + 1) pivot // This item is fine, continue looking.
    | _ -> return low + 1 // This item is too large, swap it.
}


let rec private lookForSmallItemAbovePivot high pivot : Algorithm<int32> = algorithm {
    // If we're at the pivot, no need to look further. The pivot is the next item
    // to be considered for swapping.
    if high - 1 = pivot then return pivot else
    match! compare (high - 1) pivot with
    | GreaterThan -> return! lookForSmallItemAbovePivot (high - 1) pivot // The item is fine, continue looking.
    | _ -> return high - 1 // This item is too small, swap it.
}


/// Moves all items larger than the pivot that are below it into the range above the pivot and vice versa.
let rec private partition low pivot high : Algorithm<struct (int32 * int32 * int32)> = algorithm {
    // Find the next item below the pivot to be swapped (this can include the pivot).
    let! low = lookForLargeItemBelowPivot low pivot
    // Find the next item above the pivot to be swapped (this can include the pivot).
    let! high = lookForSmallItemAbovePivot high pivot
    if low < high then
        // The ranges have not crossed over, so we can continue.
        do! swap low high
        // In case the pivot has been swapped away, get its new location.
        let pivot = if low = pivot then high elif high = pivot then low else pivot
        // Continue partitioning with the new ranges.
        return! partition low pivot high
    else
        // When we're done partitioning, return the new location of the pivot
        // and the new range limits. Remember that when we get here, low >= high, so
        // high is actually the new low limit.
        return struct (high, pivot, high + 1)
}


/// Moves the belowPivot index to the left until its value no longer matches the pivot value.
/// This is done to skip items that are equal to the pivot.
let rec private moveBelowPivot low belowPivot pivot : Algorithm<int32> = algorithm {
    if belowPivot <= low then return belowPivot else
    if belowPivot = pivot then return! moveBelowPivot low (belowPivot - 1) pivot else
    match! compare belowPivot pivot with
    | Equal -> return! moveBelowPivot low (belowPivot - 1) pivot
    | _ -> return belowPivot
}


/// Moves the abovePivot index to the right until its value no longer matches the pivot value.
/// This is done to skip items that are equal to the pivot.
let rec private moveAbovePivot pivot abovePivot high : Algorithm<int32> = algorithm {
    if abovePivot >= high then return abovePivot else
    if abovePivot = pivot then return! moveAbovePivot pivot (abovePivot + 1) high else
    match! compare abovePivot pivot with
    | Equal -> return! moveAbovePivot pivot (abovePivot + 1) high
    | _ -> return abovePivot
}


/// Partitions the items into items smaller or equal than the pivot to the left of the pivot,
/// and items larger or equal than the pivot to the right of the pivot.
/// Returns the next range limits.
let private partitionIntoRanges low pivot high : Algorithm<struct (int32 * int32)> = algorithm {
    // Mark the pivot item with a P label.
    use! _pivotLabel = setLabel' (Primary pivot) "P"
    let! struct (belowPivot, pivot, abovePivot) = partition low pivot high
    // Skip items eqal to the pivot (they are in the correct locations already).
    let! belowPivot = moveBelowPivot low belowPivot pivot
    let! abovePivot = moveAbovePivot pivot abovePivot high
    return struct (belowPivot, abovePivot)
}


/// Sorts a range of up to three items manually.
/// Ranges with lengths <= 3 do not really work with a median of three
/// partitioning algorithm, so we just sort these small ranges manually.
let private manualSort low high : Algorithm = algorithm {
    match high - low with
    | 1 ->
        // Range of length 2, just compare the items and swap if necessary.
        do! compareAndSwap low high
    | 2 ->
        // Range of length 3, compare and swap all combinations.
        do! compareAndSwap low (low + 1)
        do! compareAndSwap low high
        do! compareAndSwap (low + 1) high
    | _ ->
        // Range of 1, nothing to sort.
        do ()
}


/// Perform quick sort on the given range of items.
let rec private quickSortRange low high : Algorithm = algorithm {
    if high - low <= 2 then
        // If there are at most 3 items in this range, instead of determining a median of three
        // (which might not be possible), we just sort the items manually and be done with it.
        do! manualSort low high
    else
        // Otherwise, we get the pivot as the median of three, partition the range using it,
        // and then sort the resulting ranges determined by the partition.
        let! pivot = median low high
        let! struct (belowPivot, abovePivot) = partitionIntoRanges low pivot high
        do! quickSortRange low belowPivot
        do! quickSortRange abovePivot high
}


/// https://en.wikipedia.org/wiki/Quicksort
/// https://stackoverflow.com/a/40373486/1565070
[<Algorithm "Quick Sort (Hoare, Median of Three)">]
let quickSort : Algorithm = algorithm {
    let! count = getCount
    // Sort the complete range.
    do! quickSortRange 0 (count - 1)
}
