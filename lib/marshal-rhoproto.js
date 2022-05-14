// @ts-check

// import './rnode-openapi-schema';

import { rhoBuilder } from './rhoBuilder.js';

/** @typedef {import('@endo/marshal').Encoding} Encoding */
/** @typedef {import('./rhoBuilder').Miranda} Miranda */

const { freeze: harden } = Object; // TODO: harden

const { isArray } = Array;
// const { fromEntries } = Object;

/**
 * @param {X[]} xs
 * @param {Y[]} ys
 * @returns {Array<[X, Y]>}
 * @template X
 * @template Y
 */
const zip = (xs, ys) => xs.map((x, ix) => [x, ys[ix]]);

/**
 * @param {Encoding} item
 * @param {boolean=} _shouldIndent
 * @returns {string}
 */
export const decodeToRhoTerm = (item, _shouldIndent) => {
  const builder = rhoBuilder();

  /**
   * @param {Encoding} encoding
   * @returns {Miranda}
   */
  const recur = (encoding) => {
    switch (typeof encoding) {
      case 'boolean':
        return builder.primitive(encoding);
      case 'number':
        if (!Number.isSafeInteger(encoding)) throw Error('not a safe integer');
        return builder.primitive(encoding);
      case 'bigint': {
        const data = Number(encoding);
        if (!Number.isSafeInteger(data)) throw Error('not a safe integer');
        return builder.primitive(data);
      }
      case 'string':
        return builder.primitive(encoding);
      case 'object':
        if (encoding === null) return builder.Nil();
        if ('@qclass' in encoding) {
          switch (encoding['@qclass']) {
            case 'undefined':
            case 'NaN':
            case 'Infinity':
            case '-Infinity':
              throw Error('TODO');
            case 'tagged': {
              const { tag, payload } = encoding;
              switch (tag) {
                case 'copySet':
                  throw Error('TODO');
                // return { ExprSet: { data: payload.map(recur) } };
                case 'copyMap': {
                  /** @type {{ keys: Encoding[], values: Encoding[] }} */
                  const { keys, values } = payload;
                  const stringKeys = keys.map(x => {
                    if (x !== 'string') {
                      throw Error('TODO: non-string keys');
                    }
                    return x;
                  })
                  const entries = zip(stringKeys, values.map(recur));
                  return builder.mapExpr(
                    entries.map(([key, value]) => ({ key, value })),
                  );
                }
                default:
                  throw Error('TODO');
              }
            }
            default:
              throw Error('TODO');
          }
        } else if (isArray(encoding)) {
          return builder.listExpr(encoding.map(recur));
        } else {
          const keys = Object.keys(encoding);
          const values = Object.values(encoding);
          // JS Object aka copyRecord
          if (keys.filter((x) => typeof x !== 'string').length > 0)
            throw Error('TODO: non-string keys');
          const entries = zip(keys, values.map(recur));
          // TODO: distinguish copyRecord from copyMap?
          return builder.mapExpr(
            entries.map(([key, value]) => ({ key, value })),
          );
        }
      default:
        throw Error('TODO');
    }
  };
  const m = recur(item);
  const parts = [];
  // eslint-disable-next-line no-underscore-dangle
  m._printOn({ begin: () => {}, print: (s) => parts.push(s), newline: () => [].push('\n'), end: () => {} });
  return parts.join('');
};

harden(decodeToRhoTerm);
