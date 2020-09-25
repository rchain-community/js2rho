// UNTESTED sketch
// import { E } from "@agoric/eventual-send";
// import REVAddress from "rho:rchain:REVAddress";
import { lookup } from "./lib/registry";
import { Channel } from "./lib/js2rho";
const { freeze: harden } = Object; // TODO? @agoric/harden

// TODO: move to lib
/**
 * @typedef { { message: string } } Problem
 *
 * // TODO: @agoric/nat to handle overflow
 * @typedef { number } Nat
 *
 * @typedef { number } Timestamp
 */

/**
 * @typedef { { ok: true, result: T } | { ok: false, problem: Problem } } Result<T>
 * @template T
 */

/**
 * @param { T } result
 * @returns { Result<T> }
 * @template T
 */
function Ok(result) {
  return harden({ ok: true, result });
}

/**
 * @param {Problem} problem
 * @returns { Result<T> }
 * @template T
 */
function Err(problem) {
  return harden({ ok: false, problem });
}

const ListOps = harden({
  /**
   * @param {T[]} items
   * @param {(item: T) => Promise<Result<U>>} f
   * @returns { Promise<{ results: U[], problems: Problem[]}> }
   * @template T
   * @template U
   */
  async mapCollect(items, f) {
    throw { message: "not impl" };
  },
});

/**
 * clue for js2rho to handle throw
 * @param { () => Promise<T> } thunk
 * @returns { Promise<Result<T>>}
 * @template T
 */
async function maybe(thunk) {
  try {
    return Ok(await thunk());
  } catch (err) {
    return Err(err);
  }
}

/**
 * @typedef {{
 *   deposit: (allegedPayment: unknown) => Promise<Result<void>>,
 *   getBalance: () => Promise<Nat>
 * }} Purse
 */
const REVIssuer = harden({
  /** @type { () => Promise<Purse> } */
  async makeEmptyPurse() {
    // TODO: await E(REVAddress).fromUnforgeable(refundKey);
    const self = harden({
      /** @type { (pmt: unknown) => Promise<Result<void>> } */
      async deposit(_pmt) {
        return Err({ message: "not impl" });
      },
      async getBalance() {
        throw { message: "not impl" };
      },
    });
    return self;
  },
});

export default async function main() {
  /** @type {() => Promise<{ timestamp: Timestamp }> } */
  const blockInfo = await lookup("rho:rchain:blockInfo");

  const CrowdFund = harden({
    /**
     * @param {Nat} target pledge target in 10^-8 REV
     * @param {Timestamp} deadline compared to block timestamp
     */
    make(target, deadline) {
      // TODO: rename to pledges
      /**
       * @template T
       * @typedef {{
       *   peek: () => Promise<T>,
       *   get: () => Promise<T>,
       *   put: (p: T) => void
       * }} ChannelT
       */
      /**
       * TODO: rename purses to payments? or refunds?
       * @typedef {{total: Nat, purses: Purse[]}} Holdings
       */
      /** @type { ChannelT<Holdings>} */
      const holdingsCh = Channel({ total: 0, purses: [] });

      const check = harden({
        /**
         * @param {boolean} met sense of the test
         * @returns {Promise<Result<Nat>>} current pledges
         */
        async pledgeTarget(met) {
          const { total: pledges } = await holdingsCh.peek();
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
          const { timestamp: now } = await blockInfo();
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
         * @param {Purse} refund
         * @returns { ContributorSeatT }
         */
        make(refund) {
          return harden({
            /**
             * @return {Promise<Result<Purse>>}
             */
            async withdraw() {
              const pledges = await check.pledgeTarget(false);
              switch (pledges.ok) {
                case false:
                  return pledges;
                case true: {
                  const now = await check.deadline(true);
                  switch (now.ok) {
                    case false:
                      return now;
                    case true:
                      return Ok(refund);
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
                holdings.purses,
                (pmt) => benefit.deposit(pmt)
              );
              holdingsCh.put(harden({ total: 0, purses: [] }));
              return Ok({ benefit, problems });
          }
        },
      });

      const publicFacet = harden({
        /**
         * @returns { Promise<{terms: {target: Nat, deadline: Timestamp}, pledges: Nat}> }
         */
        async status() {
          const { total: pledges } = await holdingsCh.peek();
          return harden({ terms: { target, deadline }, pledges });
        },

        /**
         * a payment is a vault from unforgeable along with the unforgeable
         * @param {unknown} pmt
         * @returns {Promise<Result<ContributorSeatT>>}
         */
        async contribute(pmt) {
          const refund = await REVIssuer.makeEmptyPurse();
          const amt = await refund.deposit(pmt);
          switch (amt.ok) {
            case false:
              return amt; // transfer failed
            case true: {
              const contributorSeat = ContributorSeat.make(refund);
              const { total: pledges, purses } = await holdingsCh.get();
              const p = await refund.getBalance();
              const more = {
                // TODO: Nat, maybe to handle overflow
                total: pledges + p,
                purses: [...purses, refund],
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
}
