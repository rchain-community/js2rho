Section 3. **INTERPRETING CAPABILITIES** of [Policies as Types](https://arxiv.org/pdf/1307.7766.pdf)
sketches compiling JavaScript to RHO-calculus.

We've got a small amount of it working:

```
const makeMint = () => obj.verb(1, 2)
```

gets compiled to

```
for(_ <- makeMint) {
 obj!("verb", 1, 2)
}
```

See also [bounty issue 427](https://github.com/rchain/bounties/issues/427).