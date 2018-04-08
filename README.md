Section 3. **INTERPRETING CAPABILITIES** of [Policies as Types](https://arxiv.org/pdf/1307.7766.pdf)
sketches compiling JavaScript to RHO-calculus.

We've got a small amount of it working:

```
const decr = amount => {
    balance = Nat(balance - amount);
};
```

gets compiled (using fresh names taken from line and column numbers) to

```
contract decr(amount, l2c13_0) = {
  new l3c14_1 in {
    new l3c14_5, l3c18_2 in {
      new l3c18_3, l3c28_4 in {
	balance!("get", *l3c18_3) | amount!("get", *l3c28_4) | l3c18_2!(*l3c18_3 - *l3c28_4)
      }
      | Nat!("get", *l3c14_5) | l3c14_5!(*l3c18_2, l3c14_1)
    }
    | balance!("set", *l3c14_1, *l2c13_0)
  }
}
  | top!(Nil)
```

See also [bounty issue 427](https://github.com/rchain/bounties/issues/427).