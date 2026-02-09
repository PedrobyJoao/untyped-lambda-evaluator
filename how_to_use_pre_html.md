> this is a md format that will be converted to the html
> it refers to the **How to use** section of the page.

## How to use

Write a program with the following format:

- **any** let bindings
- **one** final expression to be evaluated

Then choose a beta-reduction strategy, optionally enable
logging of beta reduction steps, and evaluate.

> To understand more about the beta-reduction strategies,
> see the "About λ-Calculus" section.

A prelude with a few useful definitions (combinators, church numbers, etc) can be included. (link: https://github.com/PedrobyJoao/untyped-lambda-evaluator/blob/main/static/prelude.lam)

When the prelude is included, user's definitions can overwritte prelude's when they
share the same name.

Now, regarding lambda syntax: the lambda symbol can be written as either `\` or `λ` as in `\x.x` or `λx.x`

### Equational referencing

As already mentioned, it includes equational referencing, so instead of writing one
big lambda expression, expressions can be "assigned" to names (as `let id = λx.x`) and reused anywhere.

Before evaluation, references will be replaced by their pure lambda expression representation.

> PS: let definitions can _not_ be recursive.
>
> Thus `let f = λx. f x` is not allowed.

_After evaluation, we also replace the final expression with a loaded reference
if any is alpha-equivalent_.

> Possible confusion point: if prelude is included, the resultant expression
> can be something unexpected: a term defined in the prelude (such as `zero`
> which is `λf.λx.x`).

## Final notes

- It does not feature eta-reductions.
- Call-by-value (Weak, Strict) and Call-by-need (Weak, Lazy) to be implemented yet.
