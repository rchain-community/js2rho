import { Nat } from '@agoric/nat';
import { harden } from '@agoric/harden';


const Mint = harden({
    make() {
        const m = new WeakMap();
        const makePurse = () => mint(0);
        const mint = balance => {
            const purse = harden({
                getBalance() { return balance },
                makePurse,
                async deposit(amount, srcP) {
                    const src = await srcP;
                    Nat(balance + amount);
                    m.get(src)(Nat(amount));
                    balance += amount;
                },
            });
            const decr = amount => {
                balance = Nat(balance - amount);
            };
            m.set(purse, decr);
            return purse;
        };
        return mint;
    }
});
