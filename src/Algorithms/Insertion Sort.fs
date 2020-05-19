/// Contains the insertion sort implementation.
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module NikonTheThird.SortingAlgorithms.Algorithms.InsertionSort

open NikonTheThird.SortingAlgorithms.Algorithm


/// https://en.wikipedia.org/wiki/Insertion_sort
[<Algorithm "Insertion Sort">]
let insertionSort : Algorithm = algorithm {
    let! count = getCount

    // I is the next index to insert into the left part.
    for i = 1 to count - 1 do
        // Mark the current rightmost element with a label.
        use! _rightLabel = setLabel' (Primary (i - 1)) "R"

        // Move the item at i to the left until all the larger
        // items are to the right of it.
        let mutable j = i
        while algorithm { if j <= 0 then return false else return! isGreaterThan (j - 1) j } do
            do! swap (j - 1) j
            do j <- j - 1
}
