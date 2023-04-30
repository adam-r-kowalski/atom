# Fusion

```
# this is a comment

# this defines a function which squares x
square(x) = x^2

# calling f with 3 gives you 9
square(3)

# you can annotate the types of the arguments
square(x: i32) = x^2

# you can also annotate the return type
square(x: i32): i32 = x^2

# you annotate the return type but not the argument types
square(x): i32 = x^2

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
	if
		x < lb then lb
		x > ub then ub
		else x

# we can also define clamp in terms of min and max
clamp(x, lb, ub) = max(min(x, ub), lb)

# this can also be written using dot call notation
clamp(x, lb, ub) = x.min(ub).max(lb)

# the type of clamp requires a constrained generic
clamp[T: Ord](x: T, lb: T, ub: T): T =
	if
		x < lb then lb
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
```
