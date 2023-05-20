# Atom

```zig
# this is a comment

# this defines a function which squares x
fn square(x: i32) i32 = x^2

# calling f with 3 gives you 9
square(3)

# conditionals use an if then else syntax
fn max(x: i32, y: i32) i32 = if x > y then x else y

fn min(x: i32, y: i32) i32 = if x < y then x else y

# you can chain conditionals together
fn clamp(x: i32, lb: i32, ub: i32) i32 =
    if x < lb then lb
    else if x > ub then ub
    else x

# alternatively you can use a terser notation
fn clamp(x: i32, lb: i32, ub: i32) i32 =
    if x < lb then lb
       x > ub then ub
       else x

# we can also define clamp in terms of min and max
fn clamp(x: i32, lb: i32, ub: i32) i32 = max(min(x, ub), lb)

# this can also be written using dot call notation
fn clamp(x: i32, lb: i32, ub: i32) i32 = x.min(ub).max(lb)

# the type of clamp can be generalized to any type supporting ordering
fn clamp[T: Ord](x: T, lb: T, ub: T) T =
    if x < lb then lb
       x > ub then ub
       else x

# pattern matching is also done with if
fn sum(xs: i32[]) i32 =
    if xs is
        [] then 0
        [x, ...xs] then x + sum(xs)

# a fold expression can help us implement this in parallel
fn sum(xs: i32[]) i32 = fold(xs, 0, +)

# dot call notation allows for a "method" like sugar
fn sum(xs: i32[]) i32 = xs.fold(0, +)

# a naive version of fold can be written
fn fold[T, Acc](xs: T[], acc: Acc, f: Fn[[Acc, T], Acc]) Acc =
    if xs is
        [] then acc
        [x, ...xs] then
            acc = f(acc, x)
            xs.fold(acc, f)

# you can import a function from the host (here namespaced by "atom" "print")
import fn print(x: str) void

# you can export a function to the host
export fn double(x: i32) i32 = x * 2

# by default the start function is exported
fn start() void = print("hello world")

# create a unit test
test "double makes the number twice as large"
    expect double(2) == 4
    expect double(4) == 8
    expect double(5) == 10

interface Add[L, R = L]
    O: type
    fn add(x: L, y: R) O

# for expressions are a generalization of einstein summation notation
fn double_every[m](a: f32[m]) f32[m] =
    for i in a * 2

fn transpose[T, m, n](a: T[m][n]) T[n][m] =
    for i, j in a[j][i]

fn dot[T: Num, n](a: T[n], b: T[n]) T =
    for i in sum(a[i] * b[i])

fn matmul[T: Num, m, n, p](a: T[m][n], b: T[n][p]) T[m][p] =
    for i, j, k in sum(a[i][k] * b[k][j])

# sum can also be implemented using for and accumulation
fn sum[T: Add](xs: T[]) T =
    mut acc = 0
    for i in acc += xs[i]
    acc
```
