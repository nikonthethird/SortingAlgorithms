/// Contains the shell sort implementation.
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module NikonTheThird.SortingAlgorithms.Algorithms.ShellSort

open NikonTheThird.SortingAlgorithms.Algorithm


/// The gap sizes to use for the insertion part of shell sort.
let private gaps = [301; 132; 57; 23; 10; 4; 1]


/// https://en.wikipedia.org/wiki/Shellsort
[<Algorithm "Shell Sort">]
let shellSort : Algorithm = algorithm {
    let! count = getCount

    // Perform an insertion sort with decreasing gap size.
    for gap in gaps do

        // Start at the gap size (so that we can move an item at least one spot to the left).
        for i = gap to count - 1 do
            // Place a label on the current rightmost element.
            use! _rightLabel = setLabel' (Primary (i - gap)) "R"


            // Move the item at i to the left using the gap size until no more larger items
            // are to the left of it. This means the item has been sorted inside the gap window.
            let mutable j = i
            while algorithm { if j < gap then return false else return! isGreaterThan (j - gap) j } do
                do! swap (j - gap) j
                do j <- j - gap
}
