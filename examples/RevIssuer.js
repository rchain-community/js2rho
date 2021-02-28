// @ts-check

import * as _SES from "@agoric/install-ses";
import { E } from "@agoric/eventual-send";
import { RevAddress, Nat } from "./lib/rev.js";
import { escape, expect, believeMe } from "./lib/result.js";

/**
 * // TODO: @agoric/nat to handle overflow
 * @typedef { number } NatT
 */

/**
 * @typedef { import('./lib/result').Result<T>} Result<T>
 * @template T
 */

/**
 * @typedef {{
 *   deposit: (allegedPayment: unknown) => Promise<Result<unknown>>,
 *   getBalance: () => Promise<NatT>,
 * }} Purse
 * @typedef { Purse & {
 *   getAddress: () => Promise<string>,
 * }} REVPurse
 * @typedef { import('./lib/rev').REVAddress} REVAddress
 * @typedef { import('./lib/rev').Vault} Vault
 */

export default harden({
  /**
   * @param {typeof import('./lib/rev').RevVault } RevVault
   */
  make(RevVault) {
    const self = harden({
      /** @type { () => Promise<Purse> } */
      makeEmptyPurse() {
        const it = E(self).makeVaultPurse(RevVault.findOrCreate);
        return it;
      },
      /**
       * @param { typeof RevVault.findOrCreate } findOrCreate
       * @returns { Promise<Purse> }
       */
      makeVaultPurse(findOrCreate) {
        /** @type { REVAddress } */
        let myAddr;
        /** @type { Vault } */
        let myVault;

        const self = harden({
          /** @typedef { Record<string, () => Promise<unknown>> } Alleged */
          /** @type { (allegedPayment: Alleged) => Promise<Result<void>> } */
          deposit(allegedPayment) {
            const ok = Promise.all([
              E(RevVault).unforgeableAuthKey(allegedPayment),
              E(allegedPayment).getBalance(),
              E(allegedPayment).getAddress(),
            ]).then(([pmtAuthKey, allegedAmount, pmtAddr]) =>
              escape((ej) =>
                findOrCreate(pmtAddr).then((fcR) => {
                  const pmtVault = expect(ej, fcR);
                  const amount = Nat(allegedAmount);
                  return E(pmtVault)
                    .transfer(myAddr, amount, pmtAuthKey)
                    .then((txr) => expect(ej, txr));
                })
              )
            );
            return ok;
          },
          getAddress() {
            return Promise.resolve(myAddr);
          },
          getBalance() {
            return myVault.getBalance();
          },
        });
        return RevAddress.fromUnforgeable(self).then((a) => {
          myAddr = a;
          return findOrCreate(myAddr).then((found) => {
            // findOrCreate cannot fail on input from fromUnforgeable
            believeMe(found.ok);
            myVault = found.result;
            return harden(self);
          });
        });
      },
    });
    return Promise.resolve(self);
  },
});
