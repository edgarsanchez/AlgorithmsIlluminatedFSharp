let mergeWithLoop c d =
    let cLength = Array.length c
    let dLength = Array.length d
    let bLength = cLength + dLength
    let mutable i = 0
    let mutable j = 0

    let b = Array.zeroCreate bLength
    for k in 0 .. bLength - 1 do
        if i < cLength && (j >= dLength || c[i] <= d[j]) then
            b[k] <- c[i]
            i <- i + 1
        else
            b[k] <- d[j]
            j <- j + 1
    b

let mergeWithRec c d =
    let cLength = Array.length c
    let dLength = Array.length d
    let b = Array.zeroCreate (cLength + dLength)

    let rec mergeCurrent i j k =
        if i < cLength && (j >= dLength || c[i] <= d[j]) then
            b[k] <- c[i]
            mergeCurrent (i + 1) j (k + 1)
        elif j < dLength then
            b[k] <- d[j]
            mergeCurrent i (j + 1) (k + 1)
        else
            b

    mergeCurrent 0 0 0

let rec mergeSortWithLoop a =
    if Array.length a <= 1 then
        a
    else
        let halfLength = Array.length a / 2
        mergeWithLoop (mergeSortWithLoop a[.. halfLength - 1]) (mergeSortWithLoop a[halfLength ..])

let rec mergeSortWithRec a =
    if Array.length a <= 1 then
        a
    else
        let halfLength = Array.length a / 2
        mergeWithRec (mergeSortWithRec a[.. halfLength - 1]) (mergeSortWithRec a[halfLength ..])

let quickBenchmark a sorter =
    let stopWatch = System.Diagnostics.Stopwatch()
    stopWatch.Start()
    let b = sorter a
    stopWatch.Stop()

    $"Time to sort: {stopWatch.ElapsedMilliseconds} ms"


[<EntryPoint>]
let main _ =
    let rand = System.Random()
    let a = Array.init 5_000_000 (fun _ -> rand.Next())

    printfn "Merge-Sort With loop - %s" (quickBenchmark a mergeSortWithLoop)
    printfn "Merge-Sort With recursion - %s" (quickBenchmark a mergeSortWithRec)

    0
