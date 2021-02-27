const { freeze: harden } = Object; // TODO? @agoric/harden

/**
 * @typedef { { message: string } } Problem
 */

/**
 * @typedef { [ok: true, result: T] | [ok: false, problem: string] } TODOResult<T>
 * @template T
 */

/**
 * @typedef { { ok: true, result: T } | { ok: false, problem: Problem } } Result<T>
 * @template T
 */

/**
 * @param { T } result
 * @returns { Result<T> }
 * @template T
 */
export function Ok(result) {
  return harden({ ok: true, result });
}

/**
 * @param {Problem} problem
 * @returns { Result<T> }
 * @template T
 */
export function Err(problem) {
  return harden({ ok: false, problem });
}

/**
 * @typedef { (reason: unknown) => never } Ejector
 */

/**
 * @param { Ejector } ej
 * @param { Result<T> } result
 * @returns { T }
 * @template T
 */
export function expect(ej, result) {
  switch (result.ok) {
    case true:
      return result.result;
    case false:
      ej(result.problem);
      throw new TypeError("ejector must not return");
  }
}

/**
 * @param { Ejector } ej
 * @param { Result<T> } result
 * @returns { Problem }
 * @template T
 */
export function doubt(ej, result) {
  switch (result.ok) {
    case false:
      return result.problem;
    case true:
      ej(null);
      throw new TypeError("ejector must not return");
  }
}

/**
 * @param {(ej: Ejector) => Promise<T>} thunk
 * @returns { Promise<Result<T>> }
 * @template T
 */
export async function escape(thunk) {
  /** @type {Ejector} */
  const ej = (reason) => {
    throw reason;
  };
  try {
    return Ok(await thunk(ej));
  } catch (err) {
    return Err(err);
  }
}
