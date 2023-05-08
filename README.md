# Atom

```julia
# this is a comment

# this defines a function which squares x
square(x) = x^2

# calling f with 3 gives you 9
square(3)

# you can annotate the types of the arguments
square(x: i32) = x^2

# you can also annotate the return type
square(x: i32) -> i32 = x^2

# you annotate the return type but not the argument types
square(x) -> i32 = x^2

# conditionals use an if then else syntax
max(x, y) = if x > y then x else y

min(x, y) = if x < y then x else y

# you can chain conditionals together
clamp(x, lb, ub) =
    if x < lb then lb
    else if x > ub then ub
    else x

# alternatively you can use a terser notation
clamp(x, lb, ub) =
    if x < lb then lb
       x > ub then ub
       else x

# we can also define clamp in terms of min and max
clamp(x, lb, ub) = max(min(x, ub), lb)

# this can also be written using dot call notation
clamp(x, lb, ub) = x.min(ub).max(lb)

# the type of clamp requires a constrained generic
clamp[T: Ord](x: T, lb: T, ub: T) -> T =
    if x < lb then lb
       x > ub then ub
       else x

# pattern matching is also done with if
sum(xs) =
    if xs is
        [] then 0
        [x, ...xs] then x + sum(xs)


# it would be more optimal to use tail recursion
sum(xs) =
    sum_impl(xs, acc) =
        if xs is
            [] then acc
            [x, ...xs] then sum_impl(xs, acc + x)
    sum_impl(xs, 0)

# a fold expression can help us implement this in parallel
sum(xs) = fold(xs, 0, +)

# dot call notation allows for a "method" like sugar
sum(xs) = xs.fold(0, +)

# we can write type annotations here as well
sum[T](xs: T[]) -> T = xs.fold(0, +)

# you can import a function from the host (here namespaced by "atom" "print")
import print(x: str) -> void

# you can export a function to the host
export double(x: i32) -> i32 = x * 2

# by default the start function is exported
start() = print("hello world")

# create a unit test
test "double makes the number twice as large"
    expect double(2) == 4
    expect double(4) == 8
    expect double(5) == 10

# nested tests form a suite
test "suite of tests"
    test "unit test 1"
        expect double(2) == 4
        expect double(4) == 8
    test "unit test 2"
        expect double(5) == 10

interface Add[T]
    add(x: T, y: T) -> T
    zero: T

interface Sub[T: Add]
    sub(x: T, y: T) -> T

interface Mul[T]
    mul(x: T, y: T) -> T
    one: T

# for expressions are a generalization of einstein summation notation
matmul[T: Num, m, n, p](a: T[m, n], b: T[n, p]) -> T[m, p] =
    for[i, j, k] sum(a[i, k] * b[k, j])

dot[T: Num, n](a: T[n], b: T[n]) -> T =
    for[i] sum(a[i] * b[i])

transpose[T, m, n](a: T[m, n]) -> T[n, m] =
    for[i, j] a[j, i]

# sum can also be implemented using for and accumulation
sum[T: Add](xs: T[n]) -> T =
    acc := 0
    for[i] acc += xs[i]
    acc

double(x) = x + x

# method call syntax can omit the parenthesis if it's a single arg function
5.double == double(5) == 5.double()

# you can chain together methods
5.double.double == double(double(5)) == 5.double().double()
```
