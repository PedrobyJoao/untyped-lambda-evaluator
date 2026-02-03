# WIP

- [x] Alpha-renaming
- [x] Beta-reductions: Call by name, applicative, and normal order
- [x] Parser
- [x] Evaluator:
  - [x] alpha equivalence: make tests independent from alpha rename strategy
  - [x] `alphaRename` use something else rather than `'`
  - [x] Bug-fix: capture introduced, see `alphaRename`
  - [x] resolve duplicated tests for capture avoidance
  - [x] trace reductions
  - [x] eval time
- [x] Parser: Equational reference as: let name = e1 in e2 ⟶ (\name. e2) e1
  - [x] pretty error return tests
  - [x] output a bind name instead of a lam expr by checking env of let bindings
  - [x] load common combinators (be careful with conflicts if user tries to define again)
  - [x] tests for withPrelude (and returned env)
  - [x] adjust prelude (have it as a file maybe?)
- [ ] IO-Webpage
  - [x] dropdown info about lambda calculus and b reduction strategies..
  - [ ] statistics: eval time + number of beta reduction steps
  - [ ] statistics: steps
  - [ ] smaller font and more blackness on textarea part
  - [ ] check reliability of htmx/pico urls and versions
- [ ] Evaluator: hardcoded limit to the number of steps
- [ ] UI: correct docs
  - [ ] UI: link to prelude
- [ ] deploy
- [ ] CI/CD with tests and build
- [ ] README docs
- [ ] refactor Main.hs, make it modular
  - [ ] e.g.: Service module (parse + eval)
  - [ ] e2e for this general module
- [ ] Extra (I may not do it):
  - [ ] Evaluator: trace redexes insted of entire steps
  - [ ] QA: remove excess of different var names on tests, mainly `x'`, `y'`...
  - [ ] UI: automatically convert backslash to lambda
  - [ ] UI: run all b-red strategies and compare them
  - [ ] Evaluator: HOAS and de Bruijn index?
  - [ ] Parser: Syntax sugar for multiple args (`\x y z.`)
  - [ ] Statistics: memory usage  
         Memory usage stats is usually process wise with Haskell tools (e.g.: GHC RTS),
        so it's only more or less reliable if we execute the eval func in a separate process.

## Learn more here:

- https://personal.utdallas.edu/~gupta/courses/apl/lambda.pdf
- https://en.wikipedia.org/wiki/Lambda_calculus
- https://www.cs.bu.edu/fac/snyder/cs320/Lectures/Lecture15--%20Lambda%20Calculus%20II.pdf
- https://opendsa.cs.vt.edu/ODSA/Books/PL/html/ReductionStrategies.html
- https://serokell.io/blog/untyped-lambda-calculus

## Similar work:

- https://lambster.dev/
- https://lambdacalc.dev/
- https://lambdacalceval.ronaldmcorona.vercel.app/
