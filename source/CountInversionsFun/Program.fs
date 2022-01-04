let mergeAndCountSplitInv c d =
    let cLength = Array.length c
    let dLength = Array.length d
    let b = Array.zeroCreate (cLength + dLength)

    let rec mergeCurrent i j k (splitInv: uint) =
        let cItemsAvailable = i < cLength
        let dItemsAvailable = j < dLength
        if cItemsAvailable && (not dItemsAvailable || c[i] < d[j]) then
            b[k] <- c[i]
            mergeCurrent (i + 1) j (k + 1) splitInv
        elif dItemsAvailable then
            b[k] <- d[j]
            mergeCurrent i (j + 1) (k + 1) 
                (if cItemsAvailable then splitInv + uint (cLength - i) else splitInv)
        else
            b, splitInv

    mergeCurrent 0 0 0 0u

let rec sortAndCountInv a : int array * uint =
    if Array.length a <= 1 then
        a, 0u
    else
        let halfLength = Array.length a / 2
        let c, leftInv = sortAndCountInv a[.. halfLength - 1]
        let d, rightInv = sortAndCountInv a[halfLength ..]
        let b, splitInv = mergeAndCountSplitInv c d
        b, leftInv + rightInv + splitInv

let testMergeSplitInv c d =
    let b, splitInv = mergeAndCountSplitInv c d

    printfn "c = %A" c
    printfn "d = %A" d
    printfn "b = %A" b
    printfn "splitInv = %d" splitInv

let testSplitInv a =
    let b, splitInv = sortAndCountInv a

    printfn "a = %A" a
    printfn "b = %A" b
    printfn "splitInv = %d" splitInv

[<EntryPoint>]
let main _ =
    let numbers = [| 5; 3; 9; 2; 8; 1 |]
    let b, splitInv = sortAndCountInv numbers
    printfn "splitInv = %d" splitInv

    0
