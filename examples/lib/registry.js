const registry = new WeakMap();
let ix = 1;

export async function insertArbitrary(proc) {
  ix++;
  const uri = `rho:${ix}`;
  registry.set(new String(uri), proc);
  return uri;
}
