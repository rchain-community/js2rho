```
const makeMint = () => obj.verb(1, 2)
```

gets compiled to

```
for(_ <- makeMint) {
 obj!("verb", 1, 2)
}
```
