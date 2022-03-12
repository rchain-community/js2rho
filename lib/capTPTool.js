// @ts-check
import '@endo/init';
import util from 'util';
import { E, makeCapTP } from '@endo/captp';

import { decodeToRhoTypes } from './marshal-rhoproto.js';

async function main() {
  const myconn = {
    send: (msg) => {
      console.log('send:', msg);
      const rho = decodeToRhoTypes(msg);
      console.log('rho:', JSON.stringify(rho, null, 2));
    },
    onReceive: (fn) => {
      // ...
    },
  };

  const myBootstrap = {};

  // Create a message dispatcher and bootstrap.
  // Messages on myconn are exchanged with JSON-able objects.
  const { dispatch, getBootstrap, abort } = makeCapTP(
    'myid',
    myconn.send,
    myBootstrap,
  );
  myconn.onReceive = (obj) => dispatch(obj);

  // Get the remote's bootstrap object and call a remote method.
  E(getBootstrap())
    .lookup('stuff')
    .then((res) => console.log('got res', res));

  // Tear down the CapTP connection if it fails (e.g. connection is closed).
  abort(Error('Connection aborted by user.'));
}

main().catch((err) => console.error(err));
