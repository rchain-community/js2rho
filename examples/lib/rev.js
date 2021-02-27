// @ts-check
import { Ok, escape, expect } from "./result.js";

/**
 * @typedef { import('./result').Result<T>} Result<T>
 * @template T
 */

const { freeze: harden } = Object; // @agoric/harden?

export const RevAddress = (() => {
  let ix = 1;
  const vaults = new Map();
  const _next = () => {
    ix += 1;
    return `111unf${ix}`;
  };
  return harden({
    /** @type {(u: any) => Promise<string>} */
    async fromUnforgeable(u) {
      if (vaults.has(u)) {
        return vaults.get(u);
      } else {
        const v = _next();
        vaults.set(u, v);
        return v;
      }
    },
    _setUnf(addr, v) {
      vaults.set(addr, v);
    },
    _next,
  });
})();

/**
 *
 * @param {unknown} amt
 * @returns {NatT}
 * @typedef { number } NatT
 * // TODO: add ejector?
 */
export function Nat(amt) {
  if (!(typeof amt === "number" && Number.isInteger(amt) && amt >= 0)) {
    throw new Error(`not a Nat! ${amt}`);
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

  /** @type { (amount: NatT, addr: string) => Vault } */
  function mint(qty, addr) {
    let balance = qty;
    console.log("mint", { qty, addr });
    const newVault = harden({
      _addr: addr, // for debugging
      toString: () => `<rev vault @${addr}>`,
      /** @type { () => Promise<NatT> } */
      async getBalance() {
        return balance;
      },
      /** @type { (dest: REVAddress, amount: NatT, key: unknown) => Promise<Result<null>> } */
      async transfer(dest, amount, key) {
        return escape(async (ej) => {
          const destVault = expect(ej, await RevVault.findOrCreate(dest));
          const newBalance = Nat(balance - amount);
          balance = newBalance;
          // @ts-ignore
          destVault._credit(amount);
          console.log("TODO: check key; depost", amount, "into", dest);
          return null;
        });
      },
      _credit(amount) {
        balance += amount;
      },
    });

    vaults.set(addr, newVault);

    return newVault;
  }

  return harden({
    /** @type {(addr: REVAddress) => Promise<Result<Vault & ToString>> } */
    async findOrCreate(addr) {
      // TODO: validate REVAddress?

      if (vaults.has(addr)) {
        return Ok(vaults.get(addr));
      }

      const newVault = mint(0, addr);

      return Ok(newVault);
    },
    /** @type { (unf: unknown) => Promise<unknown>} */
    async unforgeableAuthKey(unf) {
      return {};
    },
    /** @type { (addr: REVAddress, amount: NatT) => Promise<Result<Vault>> } */
    async _faucet(addr, amount) {
      return Ok(mint(amount, addr));
    },
  });
})();
