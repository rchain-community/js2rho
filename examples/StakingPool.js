/**
 * StakingPool contract
 *
 * This is reverse-translated from StakingPool.rho as a use case for a
 * JavaScript-to-Rholang compiler in progress.
 *
 * TODO: back-translate test code.
 */

import { bundlePlus, tuple, console } from '@rchain-community/js2rho';
import { harden } from '@agoric/harden';
import { E } from '@agoric/eventual-send';

import deployResult from 'rho:rchain:deployId';
import RevAddress from 'rho:rev:address';
import regInsert from 'rho:registry:insertArbitrary';
import registryLookup from 'rho:registry:lookup';

/* this will be built-in

import stdout from 'rho:io:stdout';

const console = harden({
    log(it) { E(stdout)(it); },
});
 */

export default
async function main() {
    const [AuthKey, RevVault] = await Promise.all([
        // TODO: uri('rho:...') along with tuple
        E(registryLookup)('rho:rchain:authKey'),
        E(registryLookup)('rho:rchain:revVault'),
    ]);

    const StakingPool = harden({
        async create(unsealer) {
            const unf = {};
            const [revVaultAuthKey, revAddr] = await Promise.all([
                E(RevAddress).fromUnforgeable(unf),
                E(RevVault).unforgeableAuthKey(unf),
            ]);
            console.log({ "new staking pool rev addr": revAddr, "authKey": revVaultAuthKey });
            const { _0: ok, _1: vault } = await E(RevVault).findOrCreate(revAddr);
            switch (ok) {
                case true:
                    console.log({ "vault": vault });

                    /* TODO: go beyond just testing the vault mechanics... */
                    const self = harden({
                        async redeem(targetAddr, amount, sealedOrder, __return) {
                            console.log({ "redeem target": targetAddr, "amount": amount, "sealedOrder": sealedOrder });
                            const { _0: ok, _1: order } = await E(unsealer)(sealedOrder);
                            console.log({ "ok": ok, "order": order });
                            switch (ok && order === tuple(bundlePlus(self), targetAddr, amount, __return)) {
                                case true:
                                    const { _0: success, _1: msg } = await E(vault).transfer(targetAddr, amount, revVaultAuthKey);
                                    console.log({ "transfer success": success, "msg": msg });
                                    switch (success) {
                                        case true:
                                            return tuple(true, null);
                                        case false:
                                            return tuple(false, msg);
                                    }
                                case false:
                                    return tuple(false, "sealed order does not match");
                            }
                        }
                    });
                    return bundlePlus(self);
                case false:
                    return null;
            }
        },
    });
    const uri = await E(regInsert)(bundlePlus(StakingPool));
    console.log({ "StakingPool uri": uri });
    E(deployResult)(uri);
}
