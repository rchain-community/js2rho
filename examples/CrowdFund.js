// @ts-check
// import { E } from "@agoric/eventual-send";
import { Channel } from "./lib/rspace.js";
import { Ok, maybe, unwrap } from "./lib/result.js";
import * as _registry from "./lib/registry.js";
// import REVAddress from "rho:rchain:REVAddress";
import { RevAddress, Nat } from "./lib/rev.js";

const { freeze: harden } = Object; // TODO? @agoric/harden

/**
 * // TODO: @agoric/nat to handle overflow
 * @typedef { number } NatT
 *
 * @typedef { number } Timestamp
 *
 * @typedef { import('./lib/result').Problem} Problem
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
        /** @type { (pmt: unknown) => Promise<Result<unknown>> } */
        async deposit(pmt) {
          // @ts-ignore getBalan
          const amount = await pmt.getBalance();
          Nat(amount);
          // @ts-ignore
          // could do these next 2 in parallel with Promise.all
          const pmtAddr = await pmt.getAddress();
          const pmtAuthKey = await RevVault.unforgeableAuthKey(pmt);
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
         * @param {boolean} met sense of the test
         * @returns {Promise<Result<NatT>>} current pledges
         */
        async pledgeTarget(met) {
          const { balance: pledges } = await holdingsCh.peek();
          return maybe(async () => {
            if (pledges >= target === met) {
              return pledges;
            } else {
              throw harden({
                message: `funding target ${met ? "not" : ""} reached`,
                pledges,
                target,
              });
            }
          });
        },
        /**
         * @param {boolean} met sense of the test
         * @returns {Promise<Result<Timestamp>>} current time
         */
        async deadline(met) {
          const [_num, now, _sender] = await blockData();
          return maybe(async () => {
            if (now >= deadline === met) {
              return now;
            } else {
              throw harden({
                message: `deadline ${met ? "not" : ""} reached`,
                deadline,
                now,
              });
            }
          });
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
         * @returns { ContributorSeatT }
         */
        make(pledge) {
          return harden({
            /**
             * @return {Promise<Result<Purse>>}
             */
            async withdraw() {
              const pledges = await check.pledgeTarget(false);
              switch (pledges.ok) {
                case false:
                  // move message back here?
                  return pledges;
                case true: {
                  const now = await check.deadline(true);
                  switch (now.ok) {
                    case false:
                      return now;
                    case true:
                      return Ok(pledge);
                  }
                }
              }
            },
          });
        },
      });

      const beneficiarySeat = harden({
        /**
         * @returns { Promise<Result<{ benefit: Purse, problems: Problem[] }>> }
         */
        async claim() {
          const pledges = await check.pledgeTarget(true);
          switch (pledges.ok) {
            case false:
              return pledges;
            case true:
              const benefit = await REVIssuer.makeEmptyPurse();
              // commit point
              const holdings = await holdingsCh.get();
              const { problems } = await ListOps.mapCollect(
                holdings.pledges,
                (pmt) => benefit.deposit(pmt)
              );
              holdingsCh.put(harden({ balance: 0, pledges: [] }));
              return Ok({ benefit, problems });
          }
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
         * a payment is a vault from unforgeable along with the unforgeable
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
              const contributorSeat = ContributorSeat.make(pledge);
              const { balance, pledges } = await holdingsCh.get();
              const pbal = await pledge.getBalance();
              const more = {
                // TODO: refund in case of overflow?
                balance: Nat(balance + pbal),
                pledges: [...pledges, pledge],
              };
              holdingsCh.put(more);
              return Ok(contributorSeat);
            }
          }
        },
      });
      return harden({ beneficiarySeat, publicFacet });
    },
  });

  /** @type { (clock: () => Timestamp) => Promise<void> } */
  async function test(clock) {
    console.log("testing");
    const REV = 10 ** 8;
    const DAY = 24 * 60 * 60 * 1000;
    const now = clock();
    const { beneficiarySeat, publicFacet } = await CrowdFund.make(
      100000 * REV,
      now + 14 * DAY
    );
    console.log("fund:", { beneficiarySeat, publicFacet });
    console.log("claim early", await beneficiarySeat.claim());

    const ej = (problem) => {
      throw problem;
    };
    /** @type { Purse } */
    const bobPurse = await REVIssuer.makeEmptyPurse();
    const bobSeat = unwrap(await publicFacet.contribute(bobPurse), ej);
    console.log(await bobSeat.withdraw());
    console.log("claim again", await beneficiarySeat.claim());
  }

  await test(() => Date.now());

  console.log("tests passed! inserting into registry");
  // TODO: run tests; insert only if they pass
  console.log(await registry.insertArbitrary(CrowdFund));
}

main({ registry: _registry })
  .then(() => console.log("done"))
  .catch((err) => console.error(err));
