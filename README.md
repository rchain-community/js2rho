# js2rho - JavaScript to Rholang

In the Agoric architecture, a JavaScript dialect is used to write [secure smart contracts
using object capabilities](https://agoric.com/documentation/ertp/guide/#introduction).
In the RChain architecture, [Rholang](https://rholang.github.io/) is used likewise.

Section 3. **INTERPRETING CAPABILITIES** of [Policies as Types](https://arxiv.org/pdf/1307.7766.pdf)
sketches compiling JavaScript to RHO-calculus.

`js2rho` is an exploration into the feasibility of taking smart contracts
written in JavaScript and compiling them to Rholang for interoperability
with RChain.

## Getting Started

```bash
git clone https://github.com/rchain-community/js2rho.git
cd js2rho
npm install
npm test
```

## Background

See also [bounty issue 427](https://github.com/rchain/bounties/issues/427).