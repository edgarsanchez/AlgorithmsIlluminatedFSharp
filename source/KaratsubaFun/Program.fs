let add (x: int list) (y: int list) =
    let rec addCurrentDigit i j carry xPlusY =
        if i < 0 && j < 0 then
            if carry > 0 then carry :: xPlusY else xPlusY
        else
            let bruteSum = (if i >= 0 then x[i] else 0) + (if j >= 0 then y[j] else 0) + carry
            let carry, newDigit = if bruteSum >= 10 then 1, bruteSum - 10 else 0, bruteSum
            addCurrentDigit (i - 1) (j - 1) carry (newDigit :: xPlusY)

    addCurrentDigit (List.length x - 1) (List.length y - 1) 0 []

let subtract (x: int list) (y: int list) =
    let rec subtractCurrentDigit i j carry xMinusY =
        if i < 0 && j < 0 then
            if List.head xMinusY = 0 then List.tail xMinusY else xMinusY
        else
            let bruteSubstraction = (if i >= 0 then x[i] else 0) - (if j >= 0 then y[j] else 0) - carry
            let carry, newDigit = if bruteSubstraction < 0 then 1, bruteSubstraction + 10 else 0, bruteSubstraction
            subtractCurrentDigit (i - 1) (j - 1) carry (newDigit :: xMinusY)

    subtractCurrentDigit (List.length x - 1) (List.length y - 1) 0 []

let splitTail x m =
    let splitPoint = List.length x - m
    x[.. splitPoint - 1], x[splitPoint ..]

let leftShift x m =
    x @ List.replicate m 0

let multByOneDigit (x: int list) d =
    let rec multCurrentDigit i carry xByd =
        if i < 0 then
            if carry > 0 then carry :: xByd else xByd
        else
            let bruteProduct = x[i]*d
            multCurrentDigit (i - 1) (bruteProduct / 10) (bruteProduct % 10 + carry :: xByd)
    
    multCurrentDigit (List.length x - 1) 0 []

let rec karatsuba x y =
    let (+) x y = add x y
    let (-) x y = subtract x y
    let (<<<) x n = leftShift x n

    let xLength = List.length x
    let yLength = List.length y
    if yLength < 2 then
        multByOneDigit x y[0]
    elif xLength < 2 then
        multByOneDigit y x[0]
    else
        let n2 = (min xLength yLength) / 2
        let a, b = splitTail x n2
        let c, d = splitTail y n2
        let p = a + b
        let q = c + d
        let ac = karatsuba a c
        let bd = karatsuba b d
        let pq = karatsuba p q
        let adbc = pq - ac - bd
        (ac <<< n2*2) + (adbc <<< n2) + bd

        //add (add (leftShift ac (n2*2)) (leftShift adbc n2)) bd

let createNumber (digitsString: string) : (int list) =
    [ for c in digitsString -> int c - int '0' ]

let toString x =
    List.fold (fun s d -> s + string d) "" x

[<EntryPoint>]
let main _ =
    let one = createNumber "1"
    let two = createNumber "2"
    let ten = createNumber "10"
    let n15 = createNumber "15"
    let n99 = createNumber "99"
    let n123 = createNumber "123"
    let n999 = createNumber "999"
    let x = createNumber "12345"
    let y = createNumber "6789"
    let bigX = createNumber "123456789012345678901234567890"
    let bigY = createNumber "98765432109876543210"

    printfn "%s %s %s" (toString one) (toString n99) (toString (karatsuba one n99))
    printfn "%s %s %s" (toString n99) (toString one) (toString (karatsuba n99 one))
    printfn "%s %s %s" (toString two) (toString n99) (toString (karatsuba two n99))
    printfn "%s %s %s" (toString n99) (toString two) (toString (karatsuba n99 two))
    printfn "%s %s %s" (toString n99) (toString n99) (toString (karatsuba n99 n99))
    printfn "%s %s %s" (toString n15) (toString ten) (toString (karatsuba n15 ten))
    printfn "%s %s %s" (toString ten) (toString n15) (toString (karatsuba ten n15))
    printfn "%s %s %s" (toString n15) (toString n123) (toString (karatsuba n15 n123))
    printfn "%s %s %s" (toString n123) (toString n15) (toString (karatsuba n123 n15))
    printfn "%s %s %s" (toString n99) (toString n999) (toString (karatsuba n99 n999))
    printfn "%s %s %s" (toString n999) (toString n99) (toString (karatsuba n999 n99))
    printfn "%s %s %s" (toString n999) (toString n999) (toString (karatsuba n999 n999))
    printfn "%s %s %s" (toString x) (toString y) (toString (karatsuba x y))
    printfn "%s %s %s" (toString y) (toString x) (toString (karatsuba y x))
    printfn "%s %s %s" (toString bigX) (toString bigY) (toString (karatsuba bigX bigY))
    printfn "%s %s %s" (toString bigY) (toString bigX) (toString (karatsuba bigY bigX))
    printfn "%s %s %s" (toString bigX) (toString bigX) (toString (karatsuba bigX bigX))

    0