# js2rho - JavaScript to Rholang

**work in progress** by [Dan Connolly][dckc]

[dckc]: http://www.madmode.com/

In the Agoric architecture, a JavaScript dialect is used to write [secure smart contracts
using object capabilities](https://agoric.com/documentation/ertp/guide/#introduction).
In the RChain architecture, [Rholang](https://rholang.github.io/) is used likewise.

Section 3. **INTERPRETING CAPABILITIES** of [Policies as Types][1307]
sketches compiling JavaScript to RHO-calculus.

`js2rho` is an exploration into the feasibility of taking smart contracts
written in JavaScript and compiling them to Rholang for interoperability
with RChain.


## Getting Started with js2rho.js (current nodejs)

_note: tested with [nodejs](https://nodejs.org/) 13.12.0_

```bash
git clone https://github.com/rchain-community/js2rho.git
cd js2rho
npm install
npm test
```

js2rho.js is based on estree / esprima.


### Low-bandwidth install (no dev dependencies)

To install ~350K of essential dependencies without ~50M of typescript tooling, use:

```bash
npm install --only=prod
```

## Getting started with tinyses.Parser

For the [Jessie][] dialect, we have a parser and abstract syntax tree
(AST) as a scala sum-of-products type.

[Jessie]: https://github.com/Agoric/Jessie/

```bash
sbt console
scala> tinyses.TestSimpleParser.main(Array())
/home/connolly/projects/tinyses2rho
trying statement at CharSequenceReader('[', ...)
trying block at CharSequenceReader('[', ...)
block --> [1.1] failure: '{' expected but '[' found
...
```

We're evaluating escalima for await syntax.


## Cross-chain iteroperability: IBC?

Another RChain interoperability approach is to use waterken / Q style message passing
between TinySES and RChain nodes.


## Static Types

Scala should provide a good platform to explore the hypothesis that Jessie
can be easily statically typed.

## Fuzz testing Jessie

A "fuzzer" to generate arbitrary Jessie programs should be straightforward using property testing libraries.


## Background

See also [bounty issue 427][427].

[427]: https://github.com/rchain/bounties/issues/427
[1307]: https://arxiv.org/pdf/1307.7766.pdf
