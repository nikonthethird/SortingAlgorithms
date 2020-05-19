/// Contains the smooth sort implementation.
/// This algorithm is insane! All information taken from here:
/// https://www.keithschwarz.com/smoothsort/
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module NikonTheThird.SortingAlgorithms.Algorithms.SmoothSort

open NikonTheThird.SortingAlgorithms.Algorithm


/// Contains the bitvector encoding of the trees in a leonardo heap.
/// The representation is as a bitvector shifted down so that its first
/// digit is a one, along with the amount it was shifted.
type [<Struct>] private HeapShape = {
    /// A bitvector capable of holding all the leonardo numbers.
    Trees : uint64
    /// The shift amount, which is also the size of the smallest tree.
    SmallestTreeSize : int32
} 


/// Operations on the heap encoding.
[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module private HeapShape =
    let inline any { Trees = trees } =
        trees <> 0UL

    let inline get index { Trees = trees } =
        let mask = 1UL <<< index
        trees &&& mask = mask

    let inline set index value ({ Trees = trees } as this) =
        let mask = 1UL <<< index
        { this with Trees = trees |> if value then (|||) mask else (&&&) ~~~mask }

    let inline shiftLeft count ({ Trees = trees } as this) =
        { this with Trees = trees <<< count }

    let inline shiftRight count ({ Trees = trees } as this) =
        { this with Trees = trees >>> count }


/// Computes the leonardo number with the given index.
let private leonardo =
    let leonardoNumbers = ResizeArray [1; 1]
    fun x ->
        for x' = leonardoNumbers.Count to x do
            leonardoNumbers.Add (leonardoNumbers.[x' - 2] + leonardoNumbers.[x' - 1] + 1)
        leonardoNumbers.[x]


/// Returns the index to the root of that tree's second child. It's assumed that the heap
/// is well-formed and that size > 1.
let private secondChild root =
    // The second child root is always one step before the root.
    root - 1


/// Returns the index to the root of that tree's first child. It's assumed that the heap
/// is well-formed and that size > 1.
let private firstChild root size =
    // Go to the second child, then step over it.
    secondChild root - leonardo (size - 2)


/// Given an index to the root of a max-heap leonardo tree, returns
/// the index of its larger child. It's assumed that the heap is
/// well-formed and that the heap has order > 1.
let private largerChild root size : Algorithm<int32> = algorithm {
    let first = firstChild root size
    let second = secondChild root

    match! compare first second with
    | LessThan -> return second
    | _ -> return first
}


/// Given an index to the root of a single leonardo tree that
/// needs rebalancing, rebalances that tree using the standard
/// bubble-down approach.
let rec rebalanceSingleHeap root size : Algorithm = algorithm {
    // Loop until the current node has no children, which happens
    // when the order of the tree is 0 or 1.
    if size > 1 then
        let first = firstChild root size
        let second = secondChild root

        // Determine which child is larger and remember the order of its tree.
        let! struct (largerChild, childSize) = algorithm {
            match! compare first second with
            | LessThan -> return struct (second, size - 2)
            | _ -> return struct (first, size - 1)
        }

        // If the root is bigger than its child, we're done.
        match! compare root largerChild with
        | GreaterThan | Equal -> do ()
        | _ ->

        // Otherwise, swap down and update our order.
        do! swap root largerChild
        do! rebalanceSingleHeap largerChild childSize
}


/// Given an implicit leonardo heap spanning to end' that has just had an
/// element inserted into it at the very end, along with the size list for
/// that heap, rectifies the heap structure by shuffling the new root down
/// to the proper position and rebalancing the target heap.
let private leonardoHeapRectify end' shape : Algorithm = algorithm {
    // Back up one from the end' index to get to the root of the rightmost heap.
    do! struct (end' - 1, shape) |> fix (fun loop struct (itr, shape) -> algorithm {
        // Keep track of the size of the last heap that we visited. We need
        // this so that once we've positioned the new node atop the correct
        // heap we remember how large it is.
        let lastHeapSize = shape.SmallestTreeSize

        // If this is the very first heap in the tree, we're done.
        if itr = leonardo lastHeapSize - 1 then
            // Rebalance the first heap.
            do! rebalanceSingleHeap itr lastHeapSize
        else
            
            // We want to swap the previous root with this one if it's strictly
            // greater than both the root of this tree and both its children.
            // In order to avoid weird edge cases when the current heap has size
            // zero or size one, we'll compute what value will be compared against.
            let! toCompare = algorithm {
                // If we aren't an order 0 or order 1 tree, we have two children,
                // and need to check which one of the tree values is largest.
                if shape.SmallestTreeSize > 1 then
                    // Get the largest child and see if we need to change what
                    // we're comparing against.
                    let! largeChild = largerChild itr shape.SmallestTreeSize
                    // Update what element is being compared against.
                    match! compare itr largeChild with
                    | LessThan -> return largeChild
                    | _ -> return itr
                else
                    return itr
            }

            // Get the index to the root of the second heap by backing
            // up the size of this heap.
            let priorHeap = itr - leonardo lastHeapSize

            match! compare toCompare priorHeap with
            | GreaterThan | Equal ->
                // If we ran out of trees or the new tree root is less than the
                // element we're comparing, we now have the new node at the top
                // of the correct heap.
                do! rebalanceSingleHeap itr lastHeapSize
            | _ ->
                // Otherwise, do the swap and adjust our location.
                do! swap itr priorHeap

                // Scan down until we find the heap before this one. We do this
                // by continously shifting down the tree vector and bumping up
                // the size of the smallest tree until we hit a new tree.
                let! shape = shape |> fix (fun innerLoop shape -> algorithm {
                    //let shape = { shape.ShiftRight 1 with SmallestTreeSize = shape.SmallestTreeSize + 1 }
                    let shape = {
                        (shape |> HeapShape.shiftRight 1) with
                            SmallestTreeSize = shape.SmallestTreeSize + 1
                    }
                    if shape |> HeapShape.get 0 |> not then
                        return! innerLoop shape
                    else
                        return shape
                })

                do! loop struct (priorHeap, shape)
    })
}


/// Given an implicit leonardo heap spanning to end' in a range spanned to heapEnd, along
/// with the shape of the tree, increases the size of that heap by one by inserting the
/// element at the end'.
let private leonardoHeapAdd end' heapEnd shape : Algorithm<HeapShape> = algorithm {
    // There are three cases to consider, which are analogous to the cases
    // in the proof that it is possible to partition the input into heaps
    // of decreasing size:
    // 
    // Case 0: If there are no elements in the heap, add a tree of order 1.
    // Case 1: If the last two heaps have sizes that differ by one, we
    //         add the new element by merging the last two heaps.
    // Case 2: Otherwise, if the last heap has Leonardo number 1, add
    //         a singleton heap of Leonardo number 0.
    // Case 3: Otherwise, add a singleton heap of Leonardo number 1.
    let shape =
        // Case 0 represented by the first bit being a zero, it should always be 1 during
        // normal operation.
        if shape |> HeapShape.get 0 |> not then
            {
                (shape |> HeapShape.set 0 true) with
                    SmallestTreeSize = 1
            }
        // Case 1 would be represented by the last two bits of the vector both being set.
        elif (shape |> HeapShape.get 0) && (shape |> HeapShape.get 1) then
            {
                (shape |> HeapShape.shiftRight 2 |> HeapShape.set 0 true) with
                    SmallestTreeSize = shape.SmallestTreeSize + 2
            }
        // Case 2 is represented by the size of the smallest tree being 1.
        elif shape.SmallestTreeSize = 1 then
            {
                (shape |> HeapShape.shiftLeft 1 |> HeapShape.set 0 true) with
                    SmallestTreeSize = 0
            }
        // Case 3 is everything else.
        else
            {
                (shape |> HeapShape.shiftLeft (shape.SmallestTreeSize - 1) |> HeapShape.set 0 true) with
                    SmallestTreeSize = 1
            }

    // At this point, we've set up a new tree. We need to see if this tree is at the
    // final size it's going to take. If so, we'll do a full rectify on it. Otherwise,
    // all we need to do is maintain the heap property.
    let isLast =
        match shape.SmallestTreeSize with
        // If this last heap has order 0, then it's in its final position only
        // if it's the very last element of the array.
        | 0 -> end' + 1 = heapEnd
        // If this last heap has order 1, then it's in its final position if
        // it's the last element, or it's the penultimate element and it's not
        // about to be merged.
        | 1 -> end' + 1 = heapEnd || (end' + 2 = heapEnd && shape |> HeapShape.get 1 |> not)
        // Otherwise, this heap is in its final position if there isn't enough
        // room for the next leonardo number and one extra element.
        | _ -> abs (end' + 1 - heapEnd) < leonardo (shape.SmallestTreeSize - 1) + 1

    if not isLast then
        // If this isn't the final heap then just rebalance the current heap.
        do! rebalanceSingleHeap end' shape.SmallestTreeSize
    else
        // Otherwise, do a full rectify to put this node in its place.
        do! leonardoHeapRectify (end' + 1) shape

    return shape
}


/// Given an implicit leonardo heap spanning to end', along with the size list,
/// dequeues the element at end - 1 and rebalances the heap. Since the largest
/// element of the heap is already at end', this essentially keeps the max element
/// in its place and does a rebalance if necessary.
let private leonardoHeapRemove end' shape : Algorithm<HeapShape> = algorithm {
    // There are two cases to consider:
    // 
    // Case 1: The last heap is of order zero or one.  In this case,
    //         removing it doesn't expose any new trees and we can just
    //         drop it from the list of trees.
    // Case 2: The last heap is of order two or greater.  In this case,
    //         we exposed two new heaps, which may require rebalancing.
    
    // Case 1.
    if shape.SmallestTreeSize <= 1 then
        // Keep scanning up the list looking for the next tree.
        return! shape |> fix (fun loop shape -> algorithm {
            let shape = { (shape |> HeapShape.shiftRight 1) with SmallestTreeSize = shape.SmallestTreeSize + 1 }
            if shape |> HeapShape.any && shape |> HeapShape.get 0 |> not then
                return! loop shape
            else
                return shape
        })
    else

    // Break open the last heap to expose two subheaps of order k - 2 and k - 1.
    // This works by mapping the encoding (W1, n) to the encoding (W011, n - 2).
    let heapOrder = shape.SmallestTreeSize
    let shape = {
        (shape |> HeapShape.set 0 false |> HeapShape.shiftLeft 2 |> HeapShape.set 1 true |> HeapShape.set 0 true) with
            SmallestTreeSize = shape.SmallestTreeSize - 2
    }

    // We now do the insertion sort / rebalance operation on the larger exposed heap
    // to put it in its proper place, then on the smaller of the two. But first, we
    // need to find where they are. This can be done by just looking up the first and
    // second children of the former root, which was at end - 1.
    let leftHeap = firstChild (end' - 1) heapOrder
    let rightHeap = secondChild (end' - 1)

    // Rebalance the left heap. For this step we'll pretend that there is one fewer
    // heap than there actually is, since we're ignoring the rightmost heap.
    let allButLast = {
        (shape |> HeapShape.shiftRight 1) with 
            SmallestTreeSize = shape.SmallestTreeSize + 1
    }

    // We add one to the position of the left heap because the function assumes an
    // exclusive range, while leftHeap is actually the position of the root.
    do! leonardoHeapRectify (leftHeap + 1) allButLast
    do! leonardoHeapRectify (rightHeap + 1) shape

    return shape
}


/// https://www.keithschwarz.com/smoothsort/
[<Algorithm "Smooth Sort">]
let smoothSort : Algorithm = algorithm {
    let! count = getCount
    if count <= 1 then do () else

    // Construct a shape object describing the empty heap.
    let shape = { Trees = 0UL; SmallestTreeSize = 0 }

    // Convert the input into an implicit leonardo heap.
    let! shape = struct (0, shape) |> fix (fun loop struct (itr, shape) -> algorithm {
        if itr < count then
            let! shape = leonardoHeapAdd itr count shape
            return! loop struct (itr + 1, shape)
        else
            return shape
    })

    // Continuosly dequeue from the implicit leonardo heap until
    // we've consumed all the elements.
    do! struct (count, shape) |> fix (fun loop struct (itr, shape) -> algorithm {
        if itr > 0 then
            let! shape = leonardoHeapRemove itr shape
            do! loop struct (itr - 1, shape)
    })
}
