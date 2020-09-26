// @ts-check
import { Ok } from "./result.js";

/**
 * @typedef { import('./result').Result<T>} Result<T>
 * @template T
 */

const { freeze: harden } = Object; // @agoric/harden?

let ix = 1;

export const RevAddress = harden({
  async fromUnforgeable(u) {
    ix += 1;
    return `111ABC${ix}`;
  },
});

/**
 *
 * @param {unknown} amt
 * @returns {NatT}
 * @typedef { number } NatT
 */
export function Nat(amt) {
  if (!(typeof amt === "number" && amt >= 0 && Object.is(amt | 0, amt))) {
    throw new Error("not a Nat!");
  }
  return amt;
}

/**
 * @typedef {{
 *   getBalance: () => Promise<NatT>,
 *   transfer: (dest: REVAddress, amount: NatT, key: unknown) => Promise<Result<unknown>>
 * }} Vault
 *
 * @typedef {{
 *   toString: () => string
 * }} ToString
 *
 * @typedef { string } REVAddress
 */
export const RevVault = (() => {
  /** @type {Map<String, Vault>} */
  const vaults = new Map();

  return harden({
    /** @type {(addr: REVAddress) => Promise<Result<Vault & ToString>> } */
    async findOrCreate(addr) {
      // TODO: validate REVAddress?

      if (vaults.has(addr)) {
        return Ok(vaults.get(addr));
      }

      let balance = 0;
      const newVault = harden({
        toString: () => `<rev vault @${addr}>`,
        /** @type { () => Promise<NatT> } */
        async getBalance() {
          return balance;
        },
        /** @type { (dest: REVAddress, amount: NatT, key: unknown) => Promise<Result<unknown>> } */
        async transfer(dest, amount, key) {
          const newBalance = Nat(balance - amount);
          balance = newBalance;
          console.log("TODO: check key; depost", amount, "into", dest);
          return Ok(null);
        },
      });

      vaults.set(addr, newVault);

      return Ok(newVault);
    },
    /** @type { (unf: unknown) => Promise<unknown>} */
    async unforgeableAuthKey(unf) {
      return {};
    },
  });
})();
