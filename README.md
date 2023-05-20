# Atom

```zig
# this is a comment

square = fn(x: i32) i32 { x^2 }

test "function calls" {
    assert(square(1) == 1)
    assert(square(2) == 4)
    assert(square(3) == 9)
}

max = fn(x: i32, y: i32) i32 {
    if x > y { x } else { y }
}

min = fn(x: i32, y: i32) i32 {
    if x < y { x } else { y }
}

test "conditionals" {
    assert(max(5, 3) == 5)
    assert(min(5, 3) == 3)
}

clamp = fn(value: i32, low: i32, high: i32) i32 {
    if value < low { low }
    else if value > high { high }
    else { value }
}

test "chained conditionals" {
    assert(clamp(1, 3, 5) == 3)
    assert(clamp(7, 3, 5) == 5)
    assert(clamp(4, 3, 5) == 4)
}

test "named parameters" {
    assert(clamp(value=1, low=3, high=5) == 3)
    assert(clamp(value=7, low=3, high=5) == 5)
    assert(clamp(value=4, low=3, high=5) == 4)
}

test "method notation named parameters" {
    assert(1.clamp(low=3, high=5) == 3)
    assert(7.clamp(low=3, high=5) == 5)
    assert(4.clamp(low=3, high=5) == 4)
}

# there is a multi arm version of if as well
clamp = fn(x: i32, lb: i32, ub: i32) i32 {
    if {
        x < lb { lb }
        x > ub { ub }
        else { x }
    }
}

# you can create a generic clamp function which works on any ordered type
clamp = fn[T: Ord](x: T, lb: T, ub: T) T {
    if {
        x < lb { lb }
        x > ub { ub }
        else { x }
    }
}

# we can also define clamp in terms of min and max
clamp = fn(x: i32, lb: i32, ub: i32) i32 {
    max(min(x, ub), lb)
}

# this can also be written using dot call notation
clamp = fn(x: i32, lb: i32, ub: i32) i32 {
    x.min(ub).max(lb)
}


# pattern matching is done with if expression is
sum = fn(xs: i32[]) i32 {
    if xs is {
        [] { 0 }
        [x, ...xs] { x + sum(xs) }
    }
}

test "sum" {
    assert(sum([1, 2, 3]) == 6)
}

# a fold expression can help us implement this in parallel
sum = fn(xs: i32[]) i32 {
    fold(xs, 0, +)
}

# dot call notation allows for a "method" like sugar
sum = fn(xs: i32[]) i32 {
    xs.fold(0, +)
}

# a naive version of fold can be written
fold = fn[T, Acc](xs: T[], acc: Acc, f: fn(acc: Acc, val: T) Acc) Acc {
    if xs is {
        [] { acc }
        [x, ...xs] {
            acc = f(acc, x)
            xs.fold(acc, f)
        }
    }
}

# you can import a function from the host
log = foreign_import("console", "log", fn(x: str) void)

# you can export a function to the host
foreign_export("double", fn(x: i32) i32 {
    x * 2
})

# by default the start function is exported
start = fn() void {
    log("hello world")
}


# interfaces allow you to code against different types in a uniform way
Shape = interface[T] {
    area: fn(shape: T) f32
}

Circle = struct {
    radius: f32
}

# you can import functions from other atom modules
math = import("math.atom")

implement Shape[Circle] {
    area = fn(c: Circle) f32 {
        math.pi * c.radius ^ 2
    }
}

Square = struct {
    width: f32
    height: f32
}

# here we leverage destructuring
implement Shape[Square] {
    area = fn({width, height}: Square) f32 {
        width * height
    }
}

test "area of shapes" {
    assert(area(Circle(10)) == 314)
    assert(area(Square(5, 10)) == 50)
}

double_every = fn[m](a: f32[m]) f32[m] {
    for i { a * 2 }
}

test "for expressions are a generalization of einstein summation notation" {
    assert(double_every([1, 2, 3]) == [2, 4, 6])
}

transpose = fn[T, m, n](a: T[m][n]) T[n][m] {
    for i, j { a[j][i] }
}

test "transpose" {
    a = [[1, 2, 3],
         [4, 5, 6],
         [7, 8, 9]]
    b = [[1, 4, 7],
         [2, 5, 8],
         [3, 6, 9]]
    assert(transpose(a) == b)
    assert(a.transpose() == b)
}

dot = fn[T: Num, n](a: T[n], b: T[n]) T {
    for i { sum(a[i] * b[i]) }
}

matmul = fn[T: Num, m, n, p](a: T[m][n], b: T[n][p]) T[m][p] {
    for i, j, k { sum(a[i][k] * b[k][j]) }
}
```
