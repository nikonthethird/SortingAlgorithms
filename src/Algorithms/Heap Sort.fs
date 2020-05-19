/// Contains the sift down heap sort implementation.
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module NikonTheThird.SortingAlgorithms.Algorithms.HeapSort

open NikonTheThird.SortingAlgorithms.Algorithm


/// Returns the parent index of the given index.
let private parent index = (index - 1) / 2


/// Returns the left child index of the given index.
let private leftChild index = 2 * index + 1


/// Returns the right child index of the given index.
let private rightChild index = 2 * index + 2


/// Finds the index of the largest child of the tree beginning
/// with index low. Somewhere along this path a new, smaller item
/// can be inserted.
let private leafSearch low high : Algorithm<int32> = algorithm {
    let mutable index = low
    
    // As long as there is a right child of the current index,
    // descend into it (note that if there's a right child, there
    // is also always a left child).
    while rightChild index <= high do
        match! compare (rightChild index) (leftChild index) with 
        | GreaterThan -> do index <- rightChild index
        | _ -> do index <- leftChild index

    // If no more right children are present, there might still be
    // a final left child there, by definition this would be the leaf.
    if leftChild index <= high then
        do index <- leftChild index

    return index
}


/// Places the item at the given index into the heap tree of its children.
let private siftDown index high : Algorithm = algorithm {
    /// The index of the largest child leaf.
    let! leaf = leafSearch index high

    // Find the target index for the item at index along the path of the child
    // leaf back up to the item at index itself. This is the place where we need
    // to insert the item at index.
    let! targetIndex = leaf |> fix (fun loop i -> algorithm {
        match! compare index i with
        | GreaterThan -> return! loop (parent i)
        | _ -> return i
    })

    // It is possible that the item at index is already in the correct location,
    // which would mean the targetIndex is index itself. In this case we're fine.
    if targetIndex > index then
        // Put the item into the correct spot in the heap.
        do! swap index targetIndex
        // Now we have to move all the items between index and targetIndex in the
        // path to the leaf back to the correct location.
        let mutable i = targetIndex
        while i > index do
            do! swap index (parent i)
            do i <- parent i
}


/// Builds a min-heap.
let private heapify : Algorithm = algorithm {
    let! count = getCount

    // We start at the parent of the leaf with the highest index,
    // from there we build up the heap.
    let mutable index = parent (count - 1)

    while index >= 0 do
        do! siftDown index (count - 1)
        do index <- index - 1
}


/// https://en.wikipedia.org/wiki/Heapsort
[<Algorithm "Heap Sort">]
let heapSort : Algorithm = algorithm {
    let! count = getCount

    // Build a min-heap from all the items.
    do! heapify

    // Now we know (by definition of a min-heap), that the largest
    // item is in the front, so we move it to the back and re-heapify
    // the rest again, leaving the next biggest item in front and so on.
    let mutable high = count - 1
    while high > 0 do
        // Place the largest item at the end of the unsorted part.
        do! swap 0 high
        do high <- high - 1
        // We know which item destroys the heap condition, therefore
        // we only need to sift this one item down.
        do! siftDown 0 high
}
