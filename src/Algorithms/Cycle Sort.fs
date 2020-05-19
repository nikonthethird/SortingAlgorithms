/// Contains the cycle sort implementation.
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module NikonTheThird.SortingAlgorithms.Algorithms.CycleSort

open NikonTheThird.SortingAlgorithms.Algorithm


/// https://en.wikipedia.org/wiki/Cycle_sort
[<Algorithm "Cycle Sort">]
let cycleSort : Algorithm = algorithm {
    let! count = getCount

    // Loop through the array to find cycles to rotate.
    for cycleStart = 0 to count - 2 do
        let! label = setLabel' (Primary cycleStart) "0"
        let cycleLabels = ResizeArray [label]

        let rec skipEquals pos = algorithm {
            if pos = cycleStart then return pos else
            match! compare cycleStart pos with
            | Equal -> return! skipEquals (pos + 1)
            | _ -> return pos
        }

        // Find where to put the item.
        let mutable pos = cycleStart
        for i = cycleStart + 1 to count - 1 do
            match! compare i cycleStart with
            | LessThan -> do pos <- pos + 1
            | _ -> do ()

        // If the item is already there, this is not a cycle.
        if pos = cycleStart then do label.Dispose () else

        // Otherwise, put the item there or after any duplicates.
        let! pos' = skipEquals pos
        do pos <- pos'
        do! swap cycleStart pos

        // Rotate the rest of the cycle.
        while pos <> cycleStart do
            let! label = setLabel' (Primary cycleStart) (string cycleLabels.Count)
            do cycleLabels.Add label

            // Find where to put the item.
            do pos <- cycleStart
            for i = cycleStart + 1 to count - 1 do
                match! compare i cycleStart with
                | LessThan -> do pos <- pos + 1
                | _ -> do ()

            // Put the item there or after any duplicates.
            let! pos' = skipEquals pos
            do pos <- pos'
            do! swap cycleStart pos

        // Remove all the labes of this cycle.
        for label in cycleLabels do
            do label.Dispose ()
}
