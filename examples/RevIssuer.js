// @ts-check

import harden from "@agoric/harden";
import { RevAddress, Nat } from "./lib/rev.js";
import { escape, expect } from "./lib/result.js";

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

export const REVIssuer = harden({
  /**
   * @param {typeof import('./lib/rev').RevVault } RevVault
   */
  async make(RevVault) {
    const self = harden({
      /** @type { () => Promise<Purse> } */
      async makeEmptyPurse() {
        return self.makeVaultPurse(RevVault.findOrCreate);
      },
      /** @param { typeof RevVault.findOrCreate } findOrCreate } */
      async makeVaultPurse(findOrCreate) {
        /** @type { REVAddress } */
        let myAddr;

        const self = harden({
          /** @type { (allegedPayment: unknown) => Promise<Result<unknown>> } */
          async deposit(allegedPayment) {
            const pmtAuthKey = await RevVault.unforgeableAuthKey(
              allegedPayment
            );
            // @ts-ignore
            const amount = await allegedPayment.getBalance();
            // @ts-ignore
            const pmtAddr = await allegedPayment.getAddress();
            return escape(async (ej) => {
              const pmtVault = expect(ej, await findOrCreate(pmtAddr));
              Nat(amount);
              // @ts-ignore
              return expect(
                ej,
                await pmtVault.transfer(myAddr, amount, pmtAuthKey)
              );
            });
          },
          async getAddress() {
            return myAddr;
          },
          async getBalance() {
            return myVault.getBalance();
          },
        });
        myAddr = await RevAddress.fromUnforgeable(self);
        const found = await findOrCreate(myAddr);
        if (!found.ok) {
          throw new TypeError(
            "huh? how could findOrCreate fail on unf rev addr?"
          );
        }
        const myVault = found.result;
        return harden(self);
      },
    });
    return self;
  },
});
