# Toward TinySES to Rholang for RChain Interoperability

**work in progress** by [Dan Connolly][dckc]

[dckc]: http://www.madmode.com/

[TinySES][] is a small subset of JavaScript designed so that
non-experts can use to write non-trivial non-exploitable smart contracts.

Rholang is part of the emerging [RChain Platform Architectire][2]. It is a concurrent programming language,
designed to be used to implement protocols and smart contracts.

This is an exploration of interoperation between TinySES and RChain.
So far, it's just a parser and abstract syntax tree (AST) as a scala sum-of-products type.

[TinySES]: https://github.com/Agoric/TinySES/
[2]: http://rchain-architecture.readthedocs.io/en/latest/

A compiler to Rholang, following some notes from
section 3. INTERPRETING CAPABILITIES of [Policy as Types][3] (cf. #[427] )
has some less-than-obvious parts, but should be doable.
An interpreter in scala would be easier.

Another RChain interoperability approach is to use waterken / Q style message passing
between TinySES and RChain nodes.

Scala should provide a good platform to explore the hypothesis that TinySES
can be easily statically typed.

A "fuzzer" to generate arbitrary TinySES programs should be straightforward using property testing libraries.

[3]: https://arxiv.org/pdf/1307.7766.pdf
[427]: https://github.com/rchain/bounties/issues/427
