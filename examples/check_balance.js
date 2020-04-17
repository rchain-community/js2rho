import { tuple } from '@rchain-community/js2rho';

import rl from 'rho:registry:lookup';

import E from '@agoric/eventual-send';

export default
async function main() {
    const { _0: _, _1: RevVault } = await E(rl)('rho:rchain:revVault');

    console.log("2.check_balance.rho");

    const revAddress = "%REV_ADDR";

    console.log(tuple("Accessing vault at RevAddress", revAddress));

    const { _0: ok, _1: vault } = await E(RevVault).findOrCreate(revAddress);
    switch (ok) {
        case true:
            console.log("Obtained vault, checking balance");
            const balance = await E(vault).balance();
            console.log(tuple("Balance is", balance));
    }
}