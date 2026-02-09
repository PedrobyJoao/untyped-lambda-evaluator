> this is a md format that will be converted to the html
> it refers to the **About** section of the page.

Lambda calculus is a formal system for representing computation
created by Alonzo Church in 1930s. If you think as a programming language, it's
turing complete.

As a rewriting system, untyped lambda calculus is not strongly normalizing.  
Meaning that there are terms with no normal form such as: `(\x.x x) (\x.x x)`

Lambda calculus builds terms from three pieces: variables, abstractions, and applications.

A **variable** is a name like `x`.

An **abstraction** `\x. t` is an anonymous function that binds `x` in the body `t`.

And **application** `t s` means "apply `t` to `s`" (left-associative, so `f a b`
means `(f a) b`).

## β-reduction

A _redex (reducible expression)_ is any application where the left side is an abstraction: `(\x. t) s`.

Now, **β-reduction** is the substitution step upon a redex: `(\x. t) s -> t[x := s]`

`t[x := s]` means: replace all occurrences of `s` for the free occurrences of `x` in `t`.

> Free occurence refers to a variable that is not bound to any previous abstraction.
>
> The replacement process includes something called capture-avoiding substitution.
> You may check out more about it in the "Learn more here" section.

Example:

```hs
(\x. x y) z -- a redex
-- one β-reduction
z y -- an application between two free variables
```

## Normal Form

A term is in normal form when no beta-reduction applies, i.e. it has no subexpression
of the form `(\x. t) s`, meaning it has no redexes.

For example, `(\x. x) y -> y`, and `y` is in normal form.

## Weak Head Normal Form (WHNF)

A term is in WHNF when its _head is not a beta-redex_. Redexes may still appear
inside arguments or inside lambda bodies.

> Head: the leftmost term in a left-associative application chain.
>
> Example: `f a b` means `((f a) b)`, so the head is `f`.

```hs
-- WHNF:
\x. (\y. y) x
f ((\x. x) y)
x y z

-- Not WHNF (head redex):
(\x. x) y
((\x. x) y)
((\x. x) y) z
```

## Reduction strategies

Reduction strategies differ in how they choose which redex to reduce first.

They can be _strong_ or _weak_.

_Strong_: it reduces under lambdas. It can reach normal form
if it terminates.

_Weak_: it does NOT reduce under lambdas. It's still possible
to find the normal form if the expression doesn't require reducing under lambdas).
Otherwise, the last weak reduction results in the Weak Head Normal Form (WHNF)
of the expression

A few reduction strategies are: Normal Order, Applicative, Call-by-name, and Call-by-value.

> Note: Beta‑reduction is confluent (Church–Rosser), so if a term has a normal form, it is unique.
>
> Any strategy that terminates reaches the same normal form.

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
if one exists.

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

Applicative does not guarantee reduction until normal form even if one exists.

```hs
let const = \x. \y. x
let omega = (\x. x x) (\x. x x)

const ok omega
```

Normal order fully normalizes the expression as it does not try to reduce omega.

Applicative does not terminate because it tries to reduce omega first,
getting stuck in infinite recursion.

### Call-by-name (Weak, non-strict but it may recompute reductions)

It reduces only the leftmost outermost expression until
it's a lambda, and then it applies the arguments without
reducing them.

```hs
let const = \x. \y. x
let arg = (\z. z) w

const arg

-- ONE beta-reduction step outputs:

\y. arg
-- or
\y. ((\z. z) w)

-- This stops at WHNF (a lambda), even though the body still has a redex.
-- Thus, for Call-by-Name, there are no more reductions be applied.
```

### Call-by-value (Weak, strict)

It reduces the leftmost outermost redex only after first reducing the argument to a value.
It does NOT reduce under lambdas as Call-by-Name.

```hs
let f = \x. \y. (\z. z) y
let arg = (\u. u) (\v. v)

f arg

-- one step: call-by-value reduces the argument first
(\x. \y. (\z. z) y) (\v. v) -- or f (\v.v)
-- one step: then applies
\y. (\z. z) y -- WHNF
-- stops at WHNF (lambda head), even though the body still has a redex.
```

## Learn more here:

- https://serokell.io/blog/untyped-lambda-calculus
- https://en.wikipedia.org/wiki/Lambda_calculus
- https://personal.utdallas.edu/~gupta/courses/apl/lambda.pdf
- https://www.cs.bu.edu/fac/snyder/cs320/Lectures/Lecture15--%20Lambda%20Calculus%20II.pdf
- https://opendsa.cs.vt.edu/ODSA/Books/PL/html/ReductionStrategies.html
- https://j-hui.com/pages/normal-forms/
