problem2 = (n: i32, sum: i32, prev: i32, curr: i32) i32 {
    if curr > n { sum }
    else {
        delta = if curr % 2 == 0 { curr } else { 0 }
        problem2(n, sum + delta, curr, prev + curr)
    }
}

start = () i32 {
    problem2(4000000, 0, 0, 1)
}
