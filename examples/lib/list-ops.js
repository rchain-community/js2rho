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
  throw { message: "not impl" };
}

/**
 * @typedef {{ mapCollect: typeof mapCollect }} ListOpsT
 */

export default { mapCollect };
