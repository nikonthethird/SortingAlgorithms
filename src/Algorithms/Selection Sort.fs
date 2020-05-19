/// Contains the selection sort implementation.
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module NikonTheThird.SortingAlgorithms.Algorithms.SelectionSort

open NikonTheThird.SortingAlgorithms.Algorithm


/// https://en.wikipedia.org/wiki/Selection_sort
[<Algorithm "Selection Sort">]
let selectionSort : Algorithm = algorithm {
    let! count = getCount

    for index = 0 to count - 1 do
        use! _leftLabel = setLabel' (Primary (index - 1)) "L"

        /// Keeps track of the currently found minimal item.
        let mutable minIndex = index
    
        // Check all other items above it if one is smaller.
        for otherIndex = index + 1 to count - 1 do
            match! compare otherIndex minIndex with
            | LessThan -> do minIndex <- otherIndex
            | _ -> do ()
        
        // If the smallest found item is not in the right spot, move it there.
        if minIndex <> index then
            do! swap minIndex index
}
