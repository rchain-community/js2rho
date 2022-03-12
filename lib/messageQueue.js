// 2-way reliable messaging
// https://github.com/rchain-community/rchain-api/issues/94

import { startTerm, listenAtDeployId } from './vendor/rchain-api/proxy.js';
import { RhoExpr } from './vendor/rchain-api/rho-expr.js';
import { decodeToRhoTypes } from './marshal-rhoproto.js';

const { freeze: harden } = Object; // @@TODO: @endo/init harden

export const makePeer = (validator, observer) => {
  return harden({
    makeConnection: (account) => {
      const unAcked = [];
      return harden({
        /**
         * @param { string } term
         * @param msg
         * @returns { Promise<void> } promise for message acknowledgement
         */
        send: async (msg) => {
          const term = decodeToRhoTypes(msg);
          const deploy = await startTerm(term, validator, observer, account);
          const { expr } = await listenAtDeployId(observer, deploy);
          return RhoExpr.parse(expr);
        },
        onReceive: (dispatch) => {
          console.warn(
            'TODO: start polling; call dispatch when a message is available',
          );
        },
      });
    },
  });
};
harden(makePeer);
