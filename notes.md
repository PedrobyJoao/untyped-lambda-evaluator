# Practical lambda calculus

Lambda Calculus is a _turing complete_ programming language.

**Abstraction**

`\x.x`

It means a function that takes one argument which beco

## Redex

## Beta-Reduction

Note: applicative is not guaranteed to terminate while normal order is.

### Confluence

Both full beta reduction strategies are confluent (with the exception
with the cases which applicative does not terminate): applicative and normal order.

Weakly normalizing (Call by x) strategies are not confluent.

## Alpha renaming:

### Capture Avoidance

(\x. (\y. x)) y

### Var capture being introduced

(\x. (\x_1. x)) x

## Challenges:

## Parser: equational reference

- Not allowed: recursive def such as fibo
- Not allowed: mutual recursive definition (odd and even)
