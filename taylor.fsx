open System

// Встроенная функция для ln(2 + x)
let builtinLn2PlusX x = Math.Log(2.0 + x)

// Вычисление члена ряда Тейлора (наивный способ)
let rec naiveTaylorTerm x n =
    let term = Math.Pow(-1.0, float(n-1)) * Math.Pow(x, float n) / (float n * Math.Pow(2.0, float n))
    term

// Вычисление суммы ряда Тейлора наивным способом
let naiveTaylorSeries x eps =
    let rec sum n acc =
        let term = naiveTaylorTerm x n
        if Math.Abs(term) < eps then acc, n
        else sum (n + 1) (acc + term)
    sum 1 (x / 2.0)

// Умный способ вычисления ряда Тейлора (с использованием предыдущего члена)
let smartTaylorSeries x eps =
    let rec sum n acc prevTerm =
        let term = prevTerm * -x / (float n * 2.0)
        if Math.Abs(term) < eps then acc, n
        else sum (n + 1) (acc + term) term
    sum 2 (x / 2.0) (x / 2.0)

// Функция для печати таблицы
let printTable a b step eps =
    printfn "%8s %15s %20s %10s %20s %10s" "x" "Builtin" "Smart Taylor" "# terms" "Dumb Taylor" "# terms"
    for x in [a .. step .. b] do
        let builtin = builtinLn2PlusX x
        let (smartResult, smartTerms) = smartTaylorSeries x eps
        let (naiveResult, naiveTerms) = naiveTaylorSeries x eps
        printfn "%8.2f %15.10f %20.10f %10d %20.10f %10d" x builtin smartResult smartTerms naiveResult naiveTerms

// Параметры задачи
let a = -1.0
let b = 1.0
let step = 0.2
let eps = 1e-6

// Печать результатов
printTable a b step eps
