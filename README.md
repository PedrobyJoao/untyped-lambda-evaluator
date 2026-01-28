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
- [ ] IO-Webpage
  - [x] dropdown info about lambda calculus and b reduction strategies..
  - [ ] responsive and pretty
  - [ ] statistics: steps
  - [ ] statistics: eval time
  - [ ] statistics: memory?
  - [ ] check reliability of htmx/pico urls and versions
- [ ] hardcoded limit to the number of steps
- [ ] Equational reference as: let name = e1 in e2 ⟶ (\name. e2) e1
- [ ] CI/CD with tests and build
- [ ] Extra (I may not do it):
  - [ ] UI: canonical representation (x, x_1, x_2...)
  - [ ] UI: run all b-red strategies and compare them
  - [ ] Evaluator: HOAS and de Bruijn index?
  - [ ] Parser: Syntax sugar for multiple args (`\x y z.`)

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
