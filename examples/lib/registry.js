import { RevVault } from './rev.js';

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

export async function lookup(proc) {
  if (proc == 'rho:rchain:revVault') {
    return RevVault;
  }

  // console.log('lookup', proc, registry.get(proc), registry);
  return registry.get(proc) || null;
}