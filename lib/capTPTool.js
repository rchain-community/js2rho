// @ts-check
import '@endo/init';
import { E, makeCapTP } from '@endo/captp';

import { nodeFetch } from './vendor/rchain-api/curl.js';
import { RNode } from './vendor/rchain-api/rnode.js';
import { makePeer } from './messageQueue.js';
import { makeAccount } from './vendor/rchain-api/proxy.js';

async function main({ https, setTimeout, clock, env }) {
  const fetch = nodeFetch({ http: https });
  const node = RNode(fetch);
  const validator = node.validator('https://rnodeapi.rhobot.net');
  const observer = node.observer('https://rnodeapi.rhobot.net');
  const pkHex = env.BOOTSTRAP_ACCOUNT;
  const account = makeAccount(pkHex, observer, { setTimeout, clock }, {});
  const peer = makePeer(validator, observer);
  const myconn = peer.makeConnection(account);

  const myBootstrap = {};

  // Create a message dispatcher and bootstrap.
  // Messages on myconn are exchanged with JSON-able objects.
  const { dispatch, getBootstrap, abort } = makeCapTP(
    'myid',
    myconn.send,
    myBootstrap,
  );
  myconn.onReceive((obj) => dispatch(obj));

  // Get the remote's bootstrap object and call a remote method.
  E(getBootstrap())
    .lookup('stuff')
    .then((res) => console.log('got res', res));

  // Tear down the CapTP connection if it fails (e.g. connection is closed).
  // abort(Error('Connection aborted by user.'));
}

(async () => {
  const https = await import('https');
  await main({ https, setTimeout, clock: () => Date.now(), env: process.env });
})().catch((err) => console.error(err));
