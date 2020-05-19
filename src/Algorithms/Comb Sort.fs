/// Contains the comb sort implementation.
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module NikonTheThird.SortingAlgorithms.Algorithms.CombSort

open NikonTheThird.SortingAlgorithms.Algorithm


/// https://en.wikipedia.org/wiki/Comb_sort
[<Algorithm "Comb Sort">]
let combSort : Algorithm = algorithm {
    let! count = getCount

    let shrink = 1.3
    let mutable gap = count
    let mutable sorted = false

    while not sorted do
        do gap <- double gap / shrink |> floor |> int32

        if gap <= 1 then
            do gap <- 1
            do sorted <- true
        
        for i = 0 to count - gap - 1 do
            match! compare i (i + gap) with
            | GreaterThan ->
                do! swap i (i + gap)
                do sorted <- false
            | _ -> do ()

}
