/// Contains the shear sort implementation.
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module NikonTheThird.SortingAlgorithms.Algorithms.ShearSort

open NikonTheThird.SortingAlgorithms.AsyncState
open NikonTheThird.SortingAlgorithms.Algorithm
open System


/// https://www.researchgate.net/publication/221083313_Shear_Sort_A_True_Two-Dimensional_Sorting_Techniques_for_VLSI_Networks/link/0c9605357b7fabd1ae000000/download
[<Algorithm "Shear Sort">]
let shearSort : Algorithm = algorithm {
    /// The real number of array items to sort.
    let! count = getCount

    /// Since shear sort usually works on two-dimensional square matrices,
    /// we have to calculate the nearest square the array represents.
    /// This also represents the number of colums, because the items are
    /// indexed by row in the matrix.
    let columnCount = count |> double |> sqrt |> ceil |> int32

    /// The array may have rows missing at the end, according to the number
    /// of items it's off of the percect square.
    let rowCount = double count / double columnCount |> ceil |> int32

    /// The colum lengths differ according to whether the last row is one that is
    /// sorted from left-to-right or right-to-left.
    /// If it's left-to-right, then the items of the last row are aligned to the left.
    /// If it's right-to-left, then the items of the last row are aligned to the right.
    let columnLengths = [|
        for column = 0 to columnCount - 1 do
        let itemsOnLastRow = match count % columnCount with 0 -> columnCount | x -> x
        let notInvertedOrEnd = rowCount % 2 = 1 || (columnCount - column - 1) >= itemsOnLastRow
        let invertedOrStart = rowCount % 2 = 0 || column >= itemsOnLastRow
        rowCount - Convert.ToInt32 (notInvertedOrEnd && invertedOrStart)
    |]

    /// Only the last row has fewer items than the square.
    let rowLengths = [|
        for row = 0 to rowCount - 1 do
            count - row * columnCount |> min columnCount
    |]

    /// Indexes have to be careful if the chosen row is inverted.
    let getIndex row column =
        if row % 2 = 0
        then row * columnCount + column
        else (row + 1) * columnCount - column - 1

    /// Insertion sort is used to sort rows and colums.
    /// Returns whether a swap has been performed, which is used to determine
    /// when the shear sort algorithm should stop.
    let insertionSort indexes : Algorithm<bool> = algorithm {
        return! struct (1, false) |> fix (fun outerLoop struct (i, swapped) -> algorithm {
            if i >= Array.length indexes then return swapped else
            let! swapped = struct (i, swapped) |> fix (fun innerLoop struct (j, swapped) -> algorithm {
                if j <= 0 then return swapped else
                match! compare indexes.[j - 1] indexes.[j] with
                | GreaterThan ->
                    do! swap indexes.[j - 1] indexes.[j]
                    return! innerLoop struct (j - 1, true)
                | _ ->
                    return swapped
            })
            return! outerLoop struct (i + 1, swapped)
        })
    }

    /// Sorts the rows of the matrix in alternating order.
    /// Even columns (including 0) are sorted left-to right, and
    /// odd columns are sorted right-to-left.
    let rowSort : Algorithm<bool> = algorithm {
        return! struct (0, false) |> fix (fun loop struct (row, swapped) -> algorithm {
            if row >= rowCount then return swapped else
            let! swapped' = insertionSort [|
                for column = 0 to Array.get rowLengths row - 1 do
                    getIndex row (if row % 2 = 0 then column else columnCount - column - 1)
            |]
            return! loop struct (row + 1, swapped || swapped')
        })
    }


    /// Sorts the columns of the matrix from top-to-bottom.
    let columnSort : Algorithm<bool> = algorithm {
        return! struct (0, false) |> fix (fun loop struct (column, swapped) -> algorithm {
            if column >= columnCount then return swapped else
            let! swapped' = insertionSort [|
                for row = 0 to Array.get columnLengths column - 1 do
                    getIndex row column
            |]
            return! loop struct (column + 1, swapped || swapped')
        })
    }

    /// Start with a row sort followed by a column sort, then row
    /// sort again and so on.
    /// If the column sort or any following sort returns that no swap
    /// has been performed, the matrix is sorted.
    do! rowSort |> AsyncState.Ignore
    do! () |> fix (fun loop () -> algorithm {
        match! columnSort with
        | false -> do ()
        | true ->

        match! rowSort with
        | false -> do ()
        | true ->

        do! loop ()
    })
}
