// @ts-check
// import { E } from "@agoric/eventual-send";
import { Channel } from "./lib/rspace.js";
import { Ok, escape, expect, doubt } from "./lib/result.js";
import * as _registry from "./lib/registry.js";
// import REVAddress from "rho:rchain:REVAddress";
import { RevAddress, Nat } from "./lib/rev.js";
import { parse } from "path";

const { freeze: harden } = Object; // TODO? @agoric/harden

/**
 * // TODO: @agoric/nat to handle overflow
 * @typedef { number } NatT
 *
 * @typedef { number } Timestamp
 *
 * @typedef { import('./lib/result').Problem} Problem
 * @typedef { import('./lib/result').Ejector} Ejector
 * @typedef { import('./lib/rev').REVAddress} REVAddress
 */
/**
 * @typedef { import('./lib/result').Result<T>} Result<T>
 * @template T
 */
/**
 * @typedef { import('./lib/rspace').ChannelT<T>} ChannelT<T>
 * @template T
 */

export default async function main({ registry }) {
  /**
   * @type { import('./lib/list-ops').ListOpsT}
   */
  const ListOps = await registry.lookup(`rho:lang:listOps`);
  /**
   * @type { typeof import('./lib/rev').RevVault }
   */
  const RevVault = await registry.lookup(`rho:rchain:revVault`);

  // ISSUE: actually it returns 3 separate processes. hm.
  /** @type {() => Promise<[number, Timestamp, string]> } */
  const blockData = await registry.lookup(`rho:block:data`);

  /**
   * @typedef {{
   *   deposit: (allegedPayment: unknown) => Promise<Result<unknown>>,
   *   getBalance: () => Promise<NatT>,
   *   getAddress: () => Promise<string>,
   * }} Purse
   */
  const REVIssuer = harden({
    /** @type { () => Promise<Purse> } */
    async makeEmptyPurse() {
      /** @type { REVAddress } */
      let myAddr;

      const self = harden({
        /** @type { (allegedPayment: unknown) => Promise<Result<unknown>> } */
        async deposit(allegedPayment) {
          const pmtAuthKey = await RevVault.unforgeableAuthKey(allegedPayment);
          // @ts-ignore
          const amount = await allegedPayment.getBalance();
          Nat(amount);
          // @ts-ignore
          // could do these next 2 in parallel with Promise.all
          const pmtAddr = await allegedPayment.getAddress();
          const result = await myVault.transfer(pmtAddr, amount, pmtAuthKey);
          return result;
        },
        async getAddress() {
          return myAddr;
        },
        async getBalance() {
          return myVault.getBalance();
        },
      });
      myAddr = await RevAddress.fromUnforgeable(self);
      const found = await RevVault.findOrCreate(myAddr);
      if (!found.ok) {
        throw new TypeError(
          "huh? how could findOrCreate fail on unf rev addr?"
        );
      }
      const myVault = found.result;
      return self;
    },
  });

  const CrowdFund = harden({
    /**
     * @param {NatT} target pledge target in 10^-8 REV
     * @param {Timestamp} deadline compared to block timestamp
     * // TODO: how reliable is block timestamp?
     *
     * TODO: parameterize issuer, esp for testing.
     * TODO: add minimum pledge threshold to guard against
     *       having to spend more to withdraw pledges
     *       than they are worth. (CRITICAL SECURITY ISSUE)
     */
    async make(target, deadline) {
      // TODO: rename to pledges
      /**
       * @typedef {{balance: NatT, pledges: Purse[]}} Holdings
       */
      /** @type { ChannelT<Holdings>} */
      const holdingsCh = Channel({ balance: 0, pledges: [] });

      const check = harden({
        /**
         * @param {Ejector} ej
         * @param {boolean} met sense of the test
         * @param {string} message in case of failure
         * @returns {Promise<NatT>} current pledges
         */
        async pledgeTarget(ej, met, message) {
          const { balance: pledges } = await holdingsCh.peek();
          if (pledges >= target === met) {
            return pledges;
          } else {
            ej(harden({ message, pledges, target }));
          }
        },
        /**
         * @param {Ejector} ej
         * @param {boolean} met sense of the test
         * @param {string} message
         * @returns {Promise<Timestamp>} current time
         */
        async deadline(ej, met, message) {
          const [_num, now, _sender] = await blockData();
          if (now >= deadline === met) {
            return now;
          } else {
            ej(harden({ message, deadline, now }));
          }
        },
      });

      /**
       * @typedef {{
       *   withdraw: () => Promise<Result<Purse>>
       * }} ContributorSeatT
       */
      const ContributorSeat = harden({
        /**
         * @param {Purse} pledge
         * @returns { Promise<ContributorSeatT> }
         */
        async make(pledge) {
          return harden({
            /**
             * @return {Promise<Result<Purse>>}
             */
            async withdraw() {
              return escape(async (/** @type Ejector */ ej) => {
                await check.pledgeTarget(ej, false, `funding target reached`);
                await check.deadline(ej, true, `deadline not reached`);
                return pledge;
              });
            },
          });
        },
      });

      const beneficiarySeat = harden({
        /**
         * @returns { Promise<Result<{ benefit: Purse, problems: Problem[] }>> }
         */
        async claim() {
          return escape(async (/** @type { Ejector } */ ej) => {
            await check.pledgeTarget(ej, true, `funding target not reached`);
            const benefit = await REVIssuer.makeEmptyPurse();
            // commit point
            const holdings = await holdingsCh.get();
            const { problems } = await ListOps.mapCollect(
              holdings.pledges,
              (pmt) => benefit.deposit(pmt)
            );
            holdingsCh.put(harden({ balance: 0, pledges: [] }));
            return { benefit, problems };
          });
        },
      });

      const publicFacet = harden({
        /**
         * @returns { Promise<{terms: {target: NatT, deadline: Timestamp}, pledges: NatT}> }
         */
        async status() {
          const { balance: pledges } = await holdingsCh.peek();
          return harden({ terms: { target, deadline }, pledges });
        },

        /**
         * @param {unknown} pmt
         * @returns {Promise<Result<ContributorSeatT>>}
         */
        async contribute(pmt) {
          const pledge = await REVIssuer.makeEmptyPurse();
          const amt = await pledge.deposit(pmt);
          switch (amt.ok) {
            case false:
              return amt; // transfer failed
            case true: {
              const contributorSeat = await ContributorSeat.make(pledge);
              const { balance, pledges } = await holdingsCh.get();
              const pbal = await pledge.getBalance();
              holdingsCh.put({
                // TODO: refund in case of overflow?
                balance: Nat(balance + pbal),
                pledges: [...pledges, pledge],
              });
              return Ok(contributorSeat);
            }
          }
        },
      });
      return harden({ beneficiarySeat, publicFacet });
    },
  });

  /** @type { (now: Timestamp) => Promise<unknown> } */
  async function test(now) {
    console.log("testing");
    const REV = 10 ** 8;
    const DAY = 24 * 60 * 60 * 1000;
    const { beneficiarySeat, publicFacet } = await CrowdFund.make(
      100000 * REV,
      now + 14 * DAY
    );
    console.log("fund:", { beneficiarySeat, publicFacet });

    /**
     * @param { string } label
     * @param { boolean } ok
     * @param {Promise<Result<T>>} p
     * @template T
     */
    async function tc(label, ok, p) {
      const result = await p;
      const [t, f] = ok ? ["pass", "FAIL"] : ["FAIL", "pass"];
      switch (result.ok) {
        case true:
          console.log(t, ":", label, result.result);
          break;
        case false:
          console.log(f, ":", label, result.problem);
          break;
      }
    }
    return escape(async (/* @type { Ejector } */ ej) => {
      console.log();
      await tc("claim early", false, beneficiarySeat.claim());

      const bobPurse = await REVIssuer.makeEmptyPurse();
      console.log("contribute");
      const bobSeat = expect(ej, await publicFacet.contribute(bobPurse));
      console.log();
      await tc("withdraw", false, bobSeat.withdraw());
      await tc("claim", false, beneficiarySeat.claim());
    });
  }

  console.log(await test(Date.now()));

  console.log("tests passed! inserting into registry");
  // TODO: run tests; insert only if they pass
  console.log(await registry.insertArbitrary(CrowdFund));
}

main({ registry: _registry })
  .then(() => console.log("done"))
  .catch((err) => console.error(err));
