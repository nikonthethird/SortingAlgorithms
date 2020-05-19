/// Contains the top down merge sort implementation.
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module NikonTheThird.SortingAlgorithms.Algorithms.MergeSortTopDown

open NikonTheThird.SortingAlgorithms.Algorithm


/// If given a Primary array index creator, returns a Secondary array
/// index creator and vice versa.
let private swapArray (createIndex : int32 -> AlgorithmIndex) =
    match createIndex -1 with
    | Primary _ -> Secondary
    | Secondary _ -> Primary


/// Merges items from the given source array into the target array.
/// The middle is the split index.
let private topDownMerge source target low middle high : Algorithm = algorithm {
    let mutable i = low
    let mutable j = middle

    for k = low to high - 1 do
        if i < middle then
            if j >= high then
                do! swap' (source i) (target k)
                do i <- i + 1
            else
                match! compare' (source i) (source j) with
                | LessThan | Equal ->
                    do! swap' (source i) (target k)
                    do i <- i + 1
                | GreaterThan ->
                    do! swap' (source j) (target k)
                    do j <- j + 1
        else
            do! swap' (source j) (target k)
            do j <- j + 1
}


/// Split items in the source array recursively until only one item remains,
/// then transfer those to the target array and merge them back.
let rec private topDownSplitMerge source desiredTarget low high  : Algorithm = algorithm {
    if high - low = 1 then
        do! swap' (source low) (desiredTarget low)
    else
        let middle = (low + high) / 2
        do! topDownSplitMerge source (swapArray desiredTarget) low middle
        do! topDownSplitMerge source (swapArray desiredTarget) middle high
        do! topDownMerge (swapArray desiredTarget) desiredTarget low middle high
}


/// https://en.wikipedia.org/wiki/Merge_sort
[<Algorithm "Merge Sort (Top Down)">]
let mergeSort : Algorithm = algorithm {
    let! count = getCount

    /// Copy all items to the working array so they can
    /// be merged back into the primary array.
    for index = 0 to count - 1 do
        do! swap' (Primary index) (Secondary index)

    do! topDownSplitMerge Secondary Primary 0 count
}
