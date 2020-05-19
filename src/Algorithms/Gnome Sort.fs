/// Contains the gnome sort implementation.
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module NikonTheThird.SortingAlgorithms.Algorithms.GnomeSort

open NikonTheThird.SortingAlgorithms.Algorithm

/// https://en.wikipedia.org/wiki/Gnome_sort
[<Algorithm "Gnome Sort">]
let gnomeSort : Algorithm = algorithm {
    let! count = getCount

    /// The current position of the comparing "gnome".
    let mutable pos = 1

    /// Updates the position of the gnome along with its label.
    let updatePos pos' : Algorithm = algorithm {
        do! setLabel pos null
        if pos' < count then
            do! setLabel pos' "G"
        do pos <- pos'
    }

    // Go through the array from left to right.
    while pos < count  do

        // If we're at the front, move to the right.
        if pos = 0 then do! updatePos (pos + 1) else

        // The gnome compares its current position with the previous one.
        match! compare pos (pos - 1) with
        | GreaterThan | Equal ->
            // The current item is too large, keep going right.
            do! updatePos (pos + 1)
        | _ ->
            // The current item is smaller, it belongs to the left, so swap
            // it with the item on the left and take one step back to see
            // if it belongs even further to the left.
            do! swap pos (pos - 1)
            do! updatePos (pos - 1)
}
