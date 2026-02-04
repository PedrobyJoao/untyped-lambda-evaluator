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
- [x] refactor Main.hs, make it modular
  - [x] e.g.: Service module (parse + eval)
- [x] Evaluator: hardcoded limit to the number of steps
- [ ] IO-Webpage
  - [x] dropdown info about lambda calculus and b reduction strategies..
  - [ ] statistics (C2): eval time + number of beta reduction steps
    - [ ] if terminated because of stepLimit, show it
  - [ ] statistics (C3): steps
  - [ ] loading: do not let user resend if still evaluating
  - [ ] smaller font and more blackness on textarea part
  - [ ] check reliability of htmx/pico urls and versions
  - [ ] link to prelude
- [ ] UI: correct docs
- [ ] deploy
- [ ] CI/CD with tests and build
- [ ] README docs
- [ ] e2e (parser + eval)
- [ ] Extra (I may not do it):
  - [ ] UI: copy button
  - [ ] Indentation
  - [ ] Evaluator: trace redexes insted of entire steps
  - [ ] UI: run all b-red strategies and compare them
  - [ ] Eval: Call by value
  - [ ] QA: remove excess of different var names on tests, mainly `x'`, `y'`...
  - [ ] UI: automatically convert backslash to lambda
  - [ ] UI:statistics: number of alpha renamings
  - [ ] Evaluator: HOAS and de Bruijn index?
  - [ ] Parser: Syntax sugar for multiple args (`\x y z.`)
  - [ ] Optimal evaluator algorithm
    - [ ] might include memoization of subterms and their normal forms
    - [ ] Concurrent eval of `App` terms in `App e1 e2`
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
