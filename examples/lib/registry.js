import ListOps from "./list-ops.js";
import { RevVault } from "./rev.js";

// const registry = new WeakMap();
const registry = new Map();
let ix = 0;

export async function insertArbitrary(proc) {
  ix++;
  const uri = new String(`rho:${ix}`);
  registry.set(uri, proc);
  // console.log('insert results', registry);
  return uri;
}

let blockNumber = 1000;
let timestamp = 2000;

async function blockData() {
  blockNumber += 7;
  timestamp += 27 * 1000;
  const sender = "111AAbc.sender";
  return [blockNumber, timestamp, sender];
}

/**
 * @param {URI} proc
 * @typedef { string } URI
 */
export async function lookup(proc) {
  switch (proc) {
    case `rho:rchain:revVault`:
      return RevVault;
    case `rho:lang:listOps`:
      return ListOps;
    case `rho:block:data`:
      return blockData;
    default:
      // console.log('lookup', proc, registry.get(proc), registry);
      return registry.get(proc) || null;
  }
}
