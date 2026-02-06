> this is a md format that will be converted to the html
> it refers to the **About** section of the page.
>
> `##` == `h4`

Lambda calculus is a formal system for representing computation
created by Alonzo Church in 1930s. If you think as a programming language, it's
turing complete.

> As a rewriting system, untyped lambda calculus is not strongly normalizing.  
> Meaning that there are terms with no normal form such as:
>
> `(\x.x x) (\x.x x)`

And here you have a tool to play with the evaluation/normalization
of lambda calculus expressions.

## How to use

todo: prelude
todo: equational referencing confusion output

The format of your program must be:

- **0** or **n** let bindings
- **1** final expression to be evaluated

### Equational referencing

It includes equational referencing, so instead of writing one
big lambda expression, you can define certain expressions with
names (as `let id = \x.x`) and reuse them anywhere.

Before evaluation, the names will be expanded into their pure lambda
expression definition.

> PS: let binding definitions can not be recursive.
>
> Thus `let f = \x. f x` is not allowed.

## Beta-reduction

A Beta-reduction is the name given to

```hs
(\x. x y) e

```

> Note: a _redex_ is any expression in the form `(\x.t) e`

`(\x. x y) <expr>`. Here, `x` is substituted for `<expr>` in the body of the abstraction.

Thus `<expr> y`.

The tool supports evaluation with three beta-reduction strategies, two strong and one weak.

Note that beta-reduction strategies differ in how they choose which redex to reduce first.

> Strong reduction: it reduces under lambdas. It can reach normal form
> if it terminates.
>
> Weak reduction: it does NOT reduce under lambdas. It's still possible
> to find the normal form using weak reduction strategies depending on the expression (
> if they do not require reducing under lambdas).
> But if not, the last weak reduction results in a Weak Head Normal Form (WHNF).

### Normal Order (Strong, Non-Strict):

It reduces first the _leftmost outermost_ redex.

Arguments are substituted into the abstraction body before being reduced.

```hs
let f = \a. a b c
let arg = (\x.x) y -- not in normal form, it can be reduced

f arg

-- ONE beta-reduction step outputs:

arg b c
-- or
((\x.x) y) b c
```

Normal Order reduction is normalizing, meaning it's guaranteed to reduce until normal form
if such term has a normal form.

### Applicative (Strong, Strict):

It reduces first the _leftmost innermost_ redex.

Arguments are reduced first, and then substituted.

```hs
let f = \a. a b c
let arg = (\x.x) y -- not in normal form, it can be reduced

f arg

-- ONE beta-reduction step outputs:

f y
-- or
(\a. a b c) y
```

#### Applicative is not normalizing

Applicative does not guarantee to reduce until normal form if one exists.

```hs
let const = \x. \y. x
let omega = (\x. x x) (\x. x x)

const ok omega
```

Normal order fully normalizes the expression as it does not try to reduce omega.

Applicative does not terminate because it tries to reduce omega first.

### Call-by-name (Weak, non-strict but it may recompute reductions)

It reduces only the leftmost outermost expression until
it's a lambda, and then it applies the arguments without
reducing them.

## Final notes

- It does not feature eta-reductions.
- Call-by-value (Weak, Strict) and Call-by-need (Weak, Lazy) to be implemented yet.

## Learn more here:

- https://en.wikipedia.org/wiki/Lambda_calculus (html link -> wikipedia)
- https://serokell.io/blog/untyped-lambda-calculus (html link -> serokell)
- https://personal.utdallas.edu/~gupta/courses/apl/lambda.pdf
- https://www.cs.bu.edu/fac/snyder/cs320/Lectures/Lecture15--%20Lambda%20Calculus%20II.pdf
- https://opendsa.cs.vt.edu/ODSA/Books/PL/html/ReductionStrategies.html (html link -> on beta reductions)
