problem3 = (x: i32, current: i32, cutoff: i32, largest_factor: i32) i32 {
    if current > cutoff { largest_factor }
    else if x % current == 0 { problem3(x / current, current + 1, cutoff, current) }
    else { x |> problem3(current + 1, cutoff, largest_factor) }
}

start = () i32 {
    x: i32 = 13195
    cutoff = x |> convert(f32) |> sqrt() |> convert(i32)
    x |> problem3(2, cutoff, 0)
}
