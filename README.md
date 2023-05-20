# Atom

```zig
# this is a comment

# this defines a function which squares x
square = fn(x: i32) i32 { x^2 }

# calling square with 3 gives you 9
square(3)

# conditionals use an if then else syntax
max = fn(x: i32, y: i32) i32 {
    if x > y { x } else { y }
}

min = fn(x: i32, y: i32) i32 {
    if x < y { x } else { y }
}

# you can chain conditionals together
clamp = fn(x: i32, lb: i32, ub: i32) i32 {
    if x < lb { lb }
    else if x > ub { ub }
    else { x }
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

# a fold expression can help us implement this in parallel
sum = fn(xs: i32[]) i32 {
    fold(xs, 0, +)
}

# dot call notation allows for a "method" like sugar
sum = fn(xs: i32[]) i32 {
    xs.fold(0, +)
}

# a naive version of fold can be written
fold = fn[T, Acc](xs: T[], acc: Acc, f: Fn[[Acc, T], Acc]) Acc {
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


# create a unit test
test "double makes the number twice as large" {
    assert(double(2) == 4)
    assert(double(4) == 8)
    assert(double(5) == 10)
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

# for expressions are a generalization of einstein summation notation
double_every = fn[m](a: f32[m]) f32[m] {
    for i { a * 2 }
}

transpose = fn[T, m, n](a: T[m][n]) T[n][m] {
    for i, j { a[j][i] }
}

dot = fn[T: Num, n](a: T[n], b: T[n]) T {
    for i { sum(a[i] * b[i]) }
}

matmul = fn[T: Num, m, n, p](a: T[m][n], b: T[n][p]) T[m][p] {
    for i, j, k { sum(a[i][k] * b[k][j]) }
}
```
