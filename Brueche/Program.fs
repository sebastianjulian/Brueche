open System

// Sebastian Maierhofer, 22.04.2017 - gcd
// Sebastian Maierhofer, 21.05.2017 - Bruch

let rec gcd a b =
    if a < 0 then
        gcd -a b
    elif b < 0 then
        gcd a -b
    elif a = 0 || b = 0 then
        1
    elif a = b then
        a
    else
        if a > b then gcd (a%b+b) b
        else gcd a (b%a+a)

type Bruch =
    val zaehler : int
    val nenner : int

    new (z : int, n : int) =
        if z = 0 then
            { zaehler = 0; nenner = 1 }
        else
            let teiler = gcd z n
            { zaehler = z / teiler; nenner = n / teiler }

    new (ganze : int, z : int, n : int) =
        Bruch(n * ganze + z, n)
        
    static member (*) (a : Bruch, b : Bruch) =
        Bruch(a.zaehler * b.zaehler, a.nenner * b.nenner)
    
    static member (/) (a : Bruch, b : Bruch) =
        a * Bruch(b.nenner, b.zaehler)

    static member (+) (a : Bruch, b : Bruch) =
        let n = a.nenner * b.nenner
        Bruch(b.nenner * a.zaehler + a.nenner * b.zaehler, n)

    static member (-) (a : Bruch, b : Bruch) =
        a + Bruch(-b.zaehler, b.nenner)
type B = Bruch

[<EntryPoint>]
let main argv = 
    let result = gcd 24 6
    printfn "%A" result

    let r = Random()
    let mutable count = 0L
    while 1 = 1 do
        let a = r.Next(2000000000) + 1
        let b = r.Next(2000000000) + 1
        let result = gcd a b
        count <- count + 1L
        if result > 1000000 then
            //printfn "gcd(%d, %d) -> %A" a b result
            Console.WriteLine("gcd({0,15:#,#}, {1,15:#,#}) -> {2,15:#,#}       ({3:#,#})", a, b, result, count)
    0
