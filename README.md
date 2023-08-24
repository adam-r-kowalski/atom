# ⚛ Atom Programming Language

Atom is a statically typed, high-performance programming language designed for machine learning and high-performance computing.
It compiles to WebAssembly, allowing your code to run anywhere WebAssembly is supported, including web browsers and server environments.

## Getting Started

### Installation

#### Prerequisites

Before installing Atom, please ensure that you have installed [zig 0.11.0-dev.3859+88284c124](https://ziglang.org/) and [wasmer](https://wasmer.io/).
Zig is a fast and reliable language that we've used to develop Atom's compiler,
and Wasmer is the WebAssembly runtime that Atom relies on for executing your code.

#### Compiling From Source

Follow the steps below to compile Atom from its source code:

1. Clone the repository:

First, you'll need to clone the Atom repository from GitHub. You can do this using Git with the following command:

```bash
git clone git@github.com:adam-r-kowalski/atom.git
```

This command creates a copy of the Atom repository on your local machine.

2. Navigate into the repository directory:

Use the following command to navigate into the directory of the repository you just cloned:

```bash
cd atom
```

3. Run the tests:

Before proceeding, it's a good idea to run the Atom tests to ensure that everything is functioning as expected. You can do this with the following command:

```bash
zig build test
```

This command runs the tests and outputs the results. If all tests pass, you're good to proceed.

4. Build the compiler:

Once the tests have passed, you can build the Atom compiler. Use the following command to do this:

```bash
zig build
```

This command builds the Atom compiler from the source code.

5. Add the compiler to your PATH:

The final step is to add the Atom compiler to your PATH so that you can use it from any location on your system. Here is how you can do it:

```bash
export PATH=$PATH:`pwd`/zig-out/bin/
```

This command adds the directory containing the Atom compiler to your system's PATH.

Now, you have Atom installed on your system, and you're ready to start coding!

### Running a Atom program

```bash
atom source.atom
```

This will compile your Atom code into web assembly and then execute it using the wasmer runtime.
To see the generated wat code add `--wat` to the compile command.

```bash
atom source.atom --wat
```

Look at the code in the `examples` folder for inspiration.
This compiler is a work in progress so expect bugs and incomplete features.
You should NOT be using this in production yet.
However, code in the examples folder should compile and run.

### Your first Atom program

Atom has a straightforward syntax that is easy to read and write.
Here is a simple Atom program that defines a function to calculate the square of a number:

```atom
fn square(x: i32) -> i32 {
    x * x
}

test "function calls" {
    assert(square(1) == 1)
    assert(square(2) == 4)
    assert(square(3) == 9)
}
```

This program defines a function `square` and a set of tests to verify its behavior.

## Basic Constructs

### Comments

In Atom, you can insert comments in your code to provide explanations or annotations.
Comments are ignored by the compiler and do not affect the execution of the program.
You can add a comment by starting the line with a hash (`#`).

Here's an example:

```atom
// This is a single line comment

// This function calculates the square of a number
fn square(x: i32) -> i32 {
    x * x // Comments can come at the end of the line
}
```

### Functions

In Atom, you define a function using the `fn` keyword, followed by a list of parameters and their types, the return type, and then the function body.

```atom
fn max(x: i32, y: i32) -> i32 {
    if x > y { x } else { y }
}

fn min(x: i32, y: i32) -> i32 {
    if x < y { x } else { y }
}

test "if else" {
    assert(max(5, 3) == 5)
    assert(min(5, 3) == 3)
}
```

This is a function `max` that takes two parameters, `x` and `y`, and returns the greater of the two.

### Control Structures

Atom supports conditional logic with `if`, `else if` and `else` expressions.

```atom
fn clamp(value: i32, low: i32, high: i32) -> i32 {
    if value < low { low }
    else if value > high { high }
    else { value }
}

test "multi arm if" {
    assert(clamp(1, 3, 5) == 3)
    assert(clamp(7, 3, 5) == 5)
    assert(clamp(4, 3, 5) == 4)
}
```

This `clamp` function ensures that a value stays within a specific range.

### Named Arguments

Atom supports named arguments, which can improve the readability of your code. Here is an example of using named arguments:

```atom
test "named arguments" {
    assert(clamp(value=1, low=3, high=5) == 3)
    assert(clamp(value=7, low=3, high=5) == 5)
    assert(clamp(value=4, low=3, high=5) == 4)
}
```

In this example, we are calling the `clamp` function with named parameters `value`, `low`, and `high`.
This makes it clear what each parameter represents, which can be particularly helpful when dealing with
functions that have many parameters or when the purpose of a parameter isn't immediately clear from its name.

In this example, we are calling the `clamp` function using the dot syntax on an integer value.
This can make your code more readable by clearly associating a function with the data it operates on.

### Pattern Matching

Atom supports pattern matching, which is a way of checking a given sequence of tokens for the presence of the constituents of some pattern.
It's a powerful tool for working with complex data structures.

```atom
// pattern matching is done with `match expression`
fn sum(xs: []i32) -> i32 {
    match xs {
        [] { 0 }
        [x, ...xs] { x + sum(xs) }
    }
}

test "sum" {
    assert(sum([1, 2, 3]) == 6)
}
```

In the above code, `sum` is a function that takes a list of integers and returns the sum of all elements in the list.
The `match xs` construct is used for pattern matching. If the list is empty (`[]`), the function returns `0`.
If the list has at least one element (`[x, ...xs]`), the function returns the sum of the first element and the
result of the recursive call to `sum` on the rest of the list.

### Destructuring

Destructuring in Atom allows you to bind a set of variables to a corresponding set of values provided in a complex data structure,
such as a struct or array. It provides a convenient way to extract multiple values from data stored in (possibly nested) objects and arrays.

For example, consider the `Square` struct and the implementation of `Shape` interface for it:

```atom
struct Square {
    width: f32,
    height: f32
}

fn area({width, height}: Square) -> f32 {
    width * height
}

test "area of square" {
    assert(area(Square(width=10, height=5)) == 50)
}
```

In the `area` function for `Square`, `{width, height}` is a destructuring assignment:
it binds the variables `width` and `height` to the respective values in the passed `Square` object.

Another example of destructuring can be found in array pattern matching:

```atom
// pattern matching with destructuring
fn sum(xs: []i32) -> i32 {
    match xs {
        [] { 0 }
        [x, ...rest] { x + sum(rest) }
    }
}
```

In this function, `[x, ...rest]` destructures the array `xs`, binding the variable `x` to the
first element of the array and `rest` to the rest of the array.

Destructuring can make your code more readable and less error-prone by avoiding manual indexing and temporary variables.

### Shadowing

Shadowing in Atom allows you to declare a new variable with the same name as a previously declared variable.
The new variable shadows the previous one within its scope, meaning the previous variable cannot be accessed.
This is not an error in Atom; it's a feature of the language.

Here's an example:

```atom
x = 5

if true {
    x = 10 // This x shadows the x declared outside the if block
    log(x) // This will print 10
}

log(x) // This will print 5 because the shadowed x was only valid within the if block
```

In this example, `x` is shadowed within the `if` block. The `log` function within the block prints the shadowed `x`,
while the one outside the block prints the original `x`.

Shadowing can be useful when you want to reuse variable names, but be careful, as it can lead to confusion if not used judiciously.

### Foreign Function Interface

Atom supports importing and exporting functions from the host environment.

```atom
@import("console", "log")
fn log(x: str) -> void

@export("double")
fn double(x: i32) -> i32 {
    x * 2
}

// call the log function from Atom
fn start() -> void {
    log("hello world")
}
```

In this example, the `log` function is imported from the host's console, and the `double` function is exported for use by the host.

[WASI](https://wasi.dev/) stands for WebAssembly System Interface. It's an API designed by the Wasmtime project that provides access to
several operating-system-like features, including files and filesystems, Berkeley sockets, clocks, and random numbers,
that we'll be proposing for standardization.

It's designed to be independent of browsers, so it doesn't depend on Web APIs or JS, and isn't limited by the need to be compatible with JS.
And it has integrated capability-based security, so it extends WebAssembly's characteristic sandboxing to include I/O.

It is a first class citizen in Atom and by targeting this API you can ensure that your programs work across as many platforms as possible.

```atom
@import("wasi_unstable", "fd_write")
fn fd_write(fd: i32, iovs: str, iovs_len: i32, mut nwritten: i32) -> i32

stdout: i32 = 1

fn print(text: str) -> void {
    mut nwritten: i32 = undefined
    fd_write(stdout, text, 1, mut nwritten)
}

fn start() -> void {
    print("Hello, World!\n")
    print("Goodbye!")
}
```

### For expressions

Atom provides for expressions to efficiently iterate through an array and build up a new one.

```atom
// Compute the dot product of two vectors
fn dot(a: []f32, b: []f32) -> f32 {
    sum(for i { a[i] * b[i] })
}

// Perform matrix multiplication
fn matmul(a: [][]f32, b: [][]f32) -> [][]f32 {
    for i, j, k { sum(a[i][k] * b[k][j]) }
}
```

### Machine Learning

Atom is designed with machine learning in mind. For expressions allow you to express how models work across a
single example rather than dealing with batches. Here is a simple linear model implemented in Atom:

```atom
struct Linear {
    m: f64,
    b: f64
}

fn predict({m, b}: Linear, x: f64) -> f64 {
    m * x + b
}

fn sse(model: Linear, x: []f64, y: []f64) -> f64 {
    sum(for i {
        y_hat = predict(model, x[i])
        (y_hat - y[i]) ^ 2
    })
}

fn update(model: Linear, gradient: Linear, learning_rate: f64) -> Linear {
    Linear(
        m=model.m - gradient.m * learning_rate,
        b=model.b - gradient.b * learning_rate,
    )
}

fn step(model: Linear, learning_rate: f64, x: []f64, y: []f64) -> Linear {
    gradient = grad(sse)(model, x, y)
    update(model, gradient, learning_rate)
}

test "gradient descent" {
    model = Linear(m=1.0, b=0.0)
    learning_rate = 0.01
    x = [1.0, 2.0, 3.0, 4.0]
    y = [2.0, 4.0, 6.0, 8.0]
    initial_loss = sse(model, x, y)
    model = step(model, learning_rate, x, y)
    updated_loss = sse(model, x, y)
    assert(updated_loss < initial_loss)
}
```

### HTML

Atom is built to be a citizen of the web. We want to ensure you can build services which can render html
on the server side or client side.

```atom
struct Customer {
    name: str,
    age: u8,
}

customers: []Customer = [
    Customer(name="Joe", age=30),
    Customer(name="Sally", age=27)
]

fn customer_info(customer: Customer) -> str {
    html`
        <li>
            <h3>${customer.name}</h3>
            <p>${customer.age}</p>
        </li>
    `
}

fn start() -> str {
    html`
        <html>
            <head>
                <title>Customers</title>
            </head>
            <body>
                <ul>
                    ${for i { customer_info(customers[i])}}
                </ul>
            </body>
        </html>
    `
}
```

## Community

Atom is open-source and community-driven. We welcome contributions of any kind: code, documentation, design, etc.
Join our community and help us make Atom the best language for machine learning and high-performance computing!
