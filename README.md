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

# if expressions can be nested
clamp = fn(value: i32, low: i32, high: i32) i32 {
    if value < low {
        low
    } else {
        if value > high {
            high
        } else {
            value
        }
    }
}

# you can use the multi arm version of if
clamp = fn(value: i32, low: i32, high: i32) i32 {
    if {
        value < low { low }
        value > high { high }
        else { value }
    }
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

# you can create a generic clamp function which works on any ordered type
Eq = interface[T] {
    (==): fn(x: T, y: T) bool
}

Ord = interface[T: Eq] {
    (<): fn(x: T, y: T) bool
    (>): fn(x: T, y: T) bool
}

clamp = fn[T: Ord](value: T, low: T, high: T) T {
    if {
        value < low { low }
        value > high { high }
        else { value }
    }
}

# we can also define clamp in terms of min and max
clamp = fn(value: i32, low: i32, high: i32) i32 {
    max(min(value, high), low)
}

# this can also be written using dot call notation
clamp = fn(value: i32, low: i32, high: i32) i32 {
    value.min(high).max(low)
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

# conditionals with a void type can leave off the else branch
start = fn(n: i32) void {
    if (n > 5) {
        log("hello world")
    }
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

double_every = fn[m: u64](a: f32[m]) f32[m] {
    for i { a[i] * 2 }
}

test "for expressions are a generalization of einstein summation notation" {
    assert(double_every([1, 2, 3]) == [2, 4, 6])
}

transpose = fn[T, m: u64, n: u64](a: T[m][n]) T[n][m] {
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

dot = fn[T: Num, n: u64](a: T[n], b: T[n]) T {
    sum(for i { a[i] * b[i] })
}

matmul = fn[T: Num, m: u64, n: u64, p: u64](a: T[m][n], b: T[n][p]) T[m][p] {
    for i, j, k { sum(a[i][k] * b[k][j]) }
}

# here we implement a simple machine learning model
Linear = struct {
    weight: f64
    bias: f64
}

predict = fn[n: u64]({weight, bias}: Linear, x: f64[n]) -> f64[n] {
    for i { weight * x[i] + bias }
}

sse = fn[n: u64](model: Linear, x: f64[n], y: f64[n]) f64 {
    y_hat = model.predict(x)
    sum(for i { (y_hat[i] - y[i]) ^ 2 })
}

update = fn(model: Linear, gradient: Linear, learning_rate: f64) Linear {
    Linear(
        weight=model.weight - gradient.weight * learning_rate,
        bias=model.bias - gradient.bias * learning_rate,
    )
}

test "gradient descent" {
    model = Linear(weight=1.0, bias=0.0)
    learning_rate = 0.01
    x = [1.0, 2.0, 3.0, 4.0]
    y = [2.0, 4.0, 6.0, 8.0]
    initial_loss = sse(model, x, y)
    gradient = grad(sse)(model, x, y)
    model = update(model, gradient, learning_rate)
    updated_loss = sse(model, x, y)
    assert(updated_loss < initial_loss)
}
```

## Compiling from source

- Ensure you have [zig 0.11.0-dev](https://ziglang.org/download/) installed and in your path
- Ensure you have [wabt](https://github.com/WebAssembly/wabt) installed and in your path
- Ensure you have [wasmtime](https://github.com/bytecodealliance/wasmtime) installed and in your path
- Clone the repository `git clone git@github.com:adam-r-kowalski/atom.git`
- Navigate into the directory `cd atom`
- Run the tests with `zig build test` and ensure they are passing
- Build the compiler with `zig build`
- Add the compiler to your path `./zig-out/bin/`
