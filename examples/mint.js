const makeMint = () => {
    const m = WeakMap();
    const makePurse = () => mint(0);
    const mint = balance => {
        const purse = def({
            getBalance: () => balance,
            makePurse: makePurse,
            deposit:
                (amount, srcP) => Q(srcP).then(src => {
                    Nat(balance + amount);
                    m.get(src)(Nat(amount));
                    balance += amount;
                })
        });
        const decr = amount => {
            balance = Nat(balance - amount);
        };
        m.set(purse, decr);
        return purse;
    };
    return mint;
};
