/// Contains the in place merge sort implementation.
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module NikonTheThird.SortingAlgorithms.Algorithms.MergeSortInPlace

open NikonTheThird.SortingAlgorithms.Algorithm


/// Merge items from i to m and j to n.
let private wmerge i m j n w : Algorithm =
    algorithm {
        let mutable i = i
        let mutable j = j
        let mutable w = w

        // Merge elements from both m and n.
        while i < m && j < n do
            match! compare i j with
            | LessThan ->
                do! swap w i
                do i <- i + 1
            | _ ->
                do! swap w j
                do j <- j + 1
            do w <- w + 1

        // Move remaining elements from m over.
        while i < m do
            do! swap w i
            do i <- i + 1
            do w <- w + 1

        // Move remaining elements from n over.
        while j < n do
            do! swap w j
            do j <- j + 1
            do w <- w + 1
    }


/// Divide the sort space and merge afterwards.
let rec private wsort l u w : Algorithm =
    algorithm {
        if u - l > 1 then
            let m = l + (u - l) / 2
            do! imsort l m
            do! imsort m u
            do! wmerge l m m u w
        else
            let mutable l = l
            let mutable w = w
            while l < u do
                do! swap l w
                do l <- l + 1
                do w <- w + 1
    }


/// Sort the items from the lower bound l to upper bound u.
and private imsort l u : Algorithm =
    algorithm {
        if u - l > 1 then
            let m = l + (u - l) / 2
            let mutable w = l + u - m
            do! wsort l m w
            while w - l > 2 do
                let n = w
                do w <- l + (n - l + 1) / 2
                do! wsort w n l
                do! wmerge l (l + n - w) n u w
            // Perform insertion sort on the remaining two elements.
            for n = w downto l + 1 do
                let mutable m = n
                while algorithm { if m >= u then return false else return! isLessThan m (m - 1) } do
                    do! swap m (m - 1)
                    do m <- m + 1
    }


/// https://stackoverflow.com/a/15657134/1565070
[<Algorithm "Merge Sort (In Place)">]
let mergeSort : Algorithm = algorithm {
    let! count = getCount

    do! imsort 0 count
}
