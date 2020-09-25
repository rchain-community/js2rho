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
    default:
      // console.log('lookup', proc, registry.get(proc), registry);
      return registry.get(proc) || null;
  }
}
