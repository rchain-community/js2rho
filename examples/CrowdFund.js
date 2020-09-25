// UNTESTED sketch
import { E } from "@agoric/eventual-send";
import REVAddress from "rho:rchain:REVAddress";
import { lookup } from "./lib/registry";
import { Channel, RhoList, ListOps, RhoMap, tuple } from "./lib/js2rho";
const { freeze: harden } = Object;

export default async function main() {
  const blockInfo = await lookup("rho:rchain:blockInfo");

  const CrowdFund = harden({
    /**
     * @param {Int} target pledge target in 10^-8 REV
     * @param {Int} deadline compared to block timestamp
     *
     * @typedef { number } Int
     */
    make(target, deadline) {
      // TODO: rename to pledges
      const holdingsCh = Channel({ total: 0, purses: RhoList([]) });

      const beneficiarySeat = harden({
        async claim(dest) {
          const { total: pledges } = holdingsCh.peek();
          if (pledges < target) {
            return tuple(
              false,
              RhoMap({
                message: "funding target not reached",
                pledges,
                target,
              })
            );
          } else {
            const { timestamp: now } = await blockInfo();
            if (now < deadline) {
              return tuple(
                false,
                RhoMap({
                  message: "payout not available until after deadline",
                  deadline,
                  now,
                })
              );
            } else {
              // commit point
              const holdings = holdingsCh.get();
              return ListOps.map(holdings.purses, (pmt) => transfer(pmt, dest));
            }
          }
        },
      });

      const publicFacet = harden({
        status() {
          const { total: pledges } = holdingsCh.peek();
          return { terms: { target, deadline }, pledges };
        },

        /**
         * a payment is a vault from unforgeable along with the unforgeable
         * @param {{ vault: unknown, key: unknown }} pmt
         */
        async contribute(pmt) {
          const refundKey = harden({});
          const refund = await E(REVAddress).fromUnforgeable(refundKey);
          const [ok, amt] = maybe(await transfer(pmt, refund));
          if (!ok) {
            return tuple(false, amt); // transfer failed
          } else {
            const contributorSeat = harden({
              async withdraw() {
                const { total: pledges } = await holdingsCh.peek();
                if (pledges >= target) {
                  return tuple(
                    false,
                    RhoMap({
                      message:
                        "refund not available after funding target reached",
                      pledges,
                      target,
                    })
                  );
                } else {
                  const { timestamp: now } = await blockInfo();
                  if (now <= deadline) {
                    return tuple(
                      false,
                      RhoMap({
                        message: "refund not available until after deadline",
                        deadline,
                        now,
                      })
                    );
                  } else {
                    return tuple(
                      true,
                      RhoMap({ vault: refund, key: refundKey })
                    );
                  }
                }
              },
            });

            const { total: pledges, purses } = holdingsCh.get();
            // TODO: handle overflow
            const total = pledges + amt;
            const refundPmt = RhoMap({
              vault: refund,
              key: refundKey,
            });
            holdingsCh(
              RhoMap({
                total,
                purses: purses.append([refundPmt]),
              })
            );
            return tuple(
              true,
              RhoMap({
                message: "pledge accepted",
                contributorSeat,
                pledges,
              })
            );
          }
        },
      });
      return RhoMap({ beneficiarySeat, publicFacet });
    },
  });
}
