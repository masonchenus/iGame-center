# Functional Programming in CoffeeScript
# CoffeeScript supports functional programming concepts

# Higher-order functions
apply = (fn, value) -> fn(value)
applyTwice = (fn, value) -> fn(fn(value))

# Function composition
compose = (f, g) -> (x) -> f(g(x))
pipe = (fns...) -> (x) -> fns.reduce ((acc, fn) -> fn(acc)), x

# Currying
curry = (fn) ->
  curried = (args...) ->
    return fn.apply(this, args) if args.length >= fn.length
    (more...) -> curried(args..., more...)
  curried

add = curry (a, b) -> a + b
add5 = add(5)
add10 = add(10)

# Map, Filter, Reduce
numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

# Map
squared = numbers.map (x) -> x * x
doubled = numbers.map (x) -> x * 2

# Filter
evens = numbers.filter (x) -> x % 2 == 0
greaterThan5 = numbers.filter (x) -> x > 5

# Reduce
sum = numbers.reduce ((acc, x) -> acc + x), 0
product = numbers.reduce ((acc, x) -> acc * x), 1
maxValue = numbers.reduce ((acc, x) -> if x > acc then x else acc), -Infinity

# Partial application
partial = (fn, args...) ->
  (more...) -> fn(args..., more...)

# Memoization
memoize = (fn) ->
  cache = {}
  (args...) ->
    key = JSON.stringify(args)
    return cache[key] if cache[key]?
    cache[key] = fn(args...)

factorial = memoize (n) ->
  return 1 if n <= 1
  n * factorial(n - 1)

# Recursion
sumArray = (arr) ->
  return 0 if arr.length is 0
  arr[0] + sumArray(arr[1..])

# Utility functions
identity = (x) -> x
constant = (x) -> -> x
noop = ->

# Predicate functions
isEven = (x) -> x % 2 == 0
isOdd = (x) -> x % 2 != 0
isPositive = (x) -> x > 0
isNegative = (x) -> x < 0

# Function guards
when = (predicate, fn) ->
  (value) -> fn(value) if predicate(value)

# Practical example
processNumbers = pipe [
  (arr) -> arr.filter isPositive
  (arr) -> arr.map (x) -> x * 2
  (arr) -> arr.reduce ((acc, x) -> acc + x), 0
]

