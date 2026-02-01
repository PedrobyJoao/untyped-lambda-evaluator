# WIP

- [x] Alpha-renaming
- [x] Beta-reductions: Call by name, applicative, and normal order
- [x] Parser
- [ ] Evaluator:
  - [x] alpha equivalence: make tests independent from alpha rename strategy
  - [x] `alphaRename` use something else rather than `'`
  - [x] Bug-fix: capture introduced, see `alphaRename`
  - [x] resolve duplicated tests for capture avoidance
  - [ ] remove excess of different var names on tests, mainly `x'`, `y'`...
  - [x] trace reductions
  - [x] eval time
  - [ ] trace redexes
- [ ] IO-Webpage
  - [x] dropdown info about lambda calculus and b reduction strategies..
  - [ ] responsive and pretty
  - [ ] statistics: eval time
  - [ ] statistics: steps
  - [ ] check reliability of htmx/pico urls and versions
- [ ] hardcoded limit to the number of steps
- [ ] Equational reference as: let name = e1 in e2 ⟶ (\name. e2) e1
- [ ] CI/CD with tests and build
- [ ] Extra (I may not do it):
  - [ ] Evaluator: trace redexes insted of entire steps
  - [ ] UI: automatically convert backslash to lambda
  - [ ] UI: run all b-red strategies and compare them
  - [ ] Evaluator: HOAS and de Bruijn index?
  - [ ] Parser: Syntax sugar for multiple args (`\x y z.`)
  - [ ] UI: canonical representation (x, x_1, x_2...)
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
