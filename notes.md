Note: done for learning purposes, the only AI-written part is the `test`/.

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

## Alpha-renaming

# Sources:

- https://personal.utdallas.edu/~gupta/courses/apl/lambda.pdf
- https://en.wikipedia.org/wiki/Lambda_calculus
- https://www.cs.bu.edu/fac/snyder/cs320/Lectures/Lecture15--%20Lambda%20Calculus%20II.pdf
- https://opendsa.cs.vt.edu/ODSA/Books/PL/html/ReductionStrategies.html
