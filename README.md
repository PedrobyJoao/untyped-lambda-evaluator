A web application for evaluating untyped λ-calculus expressions.

It can be also used as a library but the evaluator is not so optmized.

https://lambdaeval.apemortalis.com/

## How to execute:

Requirements:

- ghc
- cabal
- (you may need zlib)

After cloning the repo, from its root, run:

```sh
cabal run
```

Open `http://localhost:3000`.

Optional: set a custom port:

```sh
PORT=8080 cabal run lambda-calculus
```

There are more than 100 tests, run with:

```sh
cabal test
```

## Contributing:

There is nothing more than Haskell and HTML/CSS here.

HTMX is used, so the server is returning outputs wrapped in html structures.

- `src/Named.hs`: the evaluator itself, reductions, data types, etc.
- `src/Parser.hs`: well, the parser.
- `src/Interpreter.hs`: it abstracts parser+evaluator steps in one.
- `src/Web/App.hs`: rest api with scotty
- `src/Web/Render.hs`: rendering of html output for htmx

You may also take a look at the [backlog](https://github.com/PedrobyJoao/untyped-lambda-evaluator/blob/main/backlog.md).

## Similar work:

- https://lambster.dev/
- https://lambdacalc.dev/
- https://lambdacalceval.ronaldmcorona.vercel.app/

## Cool content about λ-calculus:

- https://serokell.io/blog/untyped-lambda-calculus
- https://en.wikipedia.org/wiki/Lambda_calculus
- https://personal.utdallas.edu/~gupta/courses/apl/lambda.pdf
- https://www.cs.bu.edu/fac/snyder/cs320/Lectures/Lecture15--%20Lambda%20Calculus%20II.pdf
- https://opendsa.cs.vt.edu/ODSA/Books/PL/html/ReductionStrategies.html
- https://j-hui.com/pages/normal-forms/
