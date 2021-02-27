const { freeze: harden } = Object; // TODO? @agoric/harden

/**
 * @typedef { import('./result').Result<T>} Result<T>
 * @template T
 */
/**
 * @typedef { import('./result').Problem} Problem
 */

/**
 * @param {T[]} items
 * @param {(item: T) => Promise<Result<U>>} f
 * @returns { Promise<{ results: U[], problems: Problem[]}> }
 * @template T
 * @template U
 */
async function mapCollect(items, f) {
  const ea = await Promise.all(items.map(f));
  // @ts-ignore
  const results = ea.filter((r) => r.ok).map((r) => r.result);
  // @ts-ignore
  const problems = ea.filter((r) => !r.ok).map((r) => r.problem);
  return { results, problems };
}

/**
 * @typedef {{ mapCollect: typeof mapCollect }} ListOpsT
 */

export default { mapCollect };
