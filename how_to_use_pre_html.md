> this is a md format that will be converted to the html
> it refers to the **How to use** section of the page.

## How to use

todo: prelude
todo: equational referencing confusion output
todo: optionally see beta reduction steps checking the checkbox

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

## Final notes

- It does not feature eta-reductions.
- Call-by-value (Weak, Strict) and Call-by-need (Weak, Lazy) to be implemented yet.
