// 2-way reliable messaging
// https://github.com/rchain-community/rchain-api/issues/94

import { startTerm, listenAtDeployId } from './vendor/rchain-api/proxy.js';
import { RhoExpr } from './vendor/rchain-api/rho-expr.js';
import { decodeToRhoTerm } from './marshal-rhoproto.js';

const { freeze: harden } = Object; // @@TODO: @endo/init harden

const lit = (s) => JSON.stringify(s);

const sendTerm = (vatId, msgTerm) => `
new ack(\`rho:rchain:deployId\`), deployerId(\`rho:rchain:deployerId\`) in {
  match {[*deployerId, ${lit(vatId)}]} {
    {*target} => target!(${msgTerm}, *ack)
  }
}`;

export const makePeer = (validator, observer) => {
  return harden({
    /**
     * @param {*} account
     * @param {string} vatId
     * @returns
     */
    makeConnection: (account, vatId) => {
      const unAcked = [];
      return harden({
        /**
         * @param {*} msg
         * @returns { Promise<void> } promise for message acknowledgement
         */
        send: async (msg) => {
          const msgTerm = decodeToRhoTerm(msg);
          const deploy = await startTerm(
            sendTerm(vatId, msgTerm),
            validator,
            observer,
            account,
          );
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
