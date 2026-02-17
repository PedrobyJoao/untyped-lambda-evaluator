# Backlog

## Core

- [ ] fix: gets stuck when returning beta-reductions from an evaluated with too many steps
      (show is probably too slow doing so many concatenations)
  - [ ] also maybe make trace lazy again (limit to 2^12 steps, do we need to worry about memory?)
- [ ] deploy
  - [x] as-package/containerize (decided to keep cabal build)
  - [x] VPS? deploy service?
  - [x] CI/CD: (build + tests) + deployment
  - [ ] faster ci (ghc installation + build)
  - [ ] harden security

## Extra

- [ ] CI: faster (ghc installation + build)
- [ ] Eval: Call by value
- [ ] Eval: Call by need
  - [ ] UI-About: Call-by-Need
- [ ] Optimal evaluator
  - [ ] have trace building optional
  - [ ] a better algorithm
  - [ ] might include memoization of subterms and their normal forms
  - [ ] Concurrent recursion of `App` terms in `App e1 e2`
- [ ] tests: More realistic e2e tests (with more interesting examples)
- [ ] tests: for functions marked with `todo: test`
- [ ] http (App.hs) tests?
- [ ] SRI for pico/htmx urls
- [ ] UI: use Blaze-HTML in Web.Render instead of pure strings
- [ ] UI: run all b-red strategies and compare them
- [ ] Indentation
- [ ] Performance: use Text instead of String
- [ ] Evaluator: trace redexes insted of entire steps
- [ ] QA: remove excess of different var names on tests, mainly `x'`, `y'`...
- [ ] Evaluator: HOAS and de Bruijn index?
- [ ] Parser: Syntax sugar for multiple args (`\x y z.`)
- [ ] Statistics: memory usage  
       Memory usage stats is usually process wise with Haskell tools (e.g.: GHC RTS),
      so it's only more or less reliable if we execute the eval func in a separate process.
- [ ] UI:statistics: number of alpha renamings
- [ ] UI: info button for each strategy with small description for each
- [ ] CICD: use newer GHC
- [ ] QA: rename data type: BetaReduction -> ReductionStrategy

## Done

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
- [x] IO-Webpage
  - [x] dropdown info about lambda calculus and b reduction strategies..
  - [x] statistics (C2): eval time + number of beta reduction steps
    - [x] if terminated because of stepLimit, show it
  - [x] statistics (C3): steps
  - [x] checkbox for steps
  - [x] loading: do not let user resend if still evaluating
  - [x] (mobile): b-red strategies component extends itself out of the form box
  - [x] improve the dropdown info -> maybe use accordion or fix modal
  - [x] smaller font and more blackness on textarea part
  - [x] show beta-reductions -> use symbol
  - [x] link to prelude
  - [x] add WHNF to Call-by-name string strategy
  - [x] everything seems so compressed, can we resolve that?
  - [x] change color pallete
  - [x] check reliability of htmx/pico urls and versions
- [x] Eval: improve show expr
- [x] e2e (parser + eval)
- [x] UI: How to use
- [x] UI: About
- [x] README docs
- [x] UI: bottom info (repo + license?)
