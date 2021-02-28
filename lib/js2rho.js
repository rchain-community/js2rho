// @ts-check
import * as _SES from "@agoric/install-ses";
import { parse } from "./vendor/jessica/lib/translate";
import { rhoBuilder } from "./rhoBuilder.js";

export const builtins = {
  "@rchain-community/js2rho": ["bundlePlus", "tuple", "console"],
  "@agoric/nat": ["Nat"],
  "@agoric/eventual-send": ["E"],
  "@agoric/install-ses": ["*"],
  "./lib/rev.js": ["RevAddress", "Nat"],
  "./lib/result.js": ["escape", "expect", "believeMe"],
};

/** @type {vdecl} */
const rhoConsole = ["console", "rho:io:stdout"];

/**
 *
 * @param {RhoBuilder} bld
 * @returns {(js: Module) => Process}
 *
 * @typedef { import('./rhoBuilder').RhoBuilder } RhoBuilder
 * @typedef { import('./rhoBuilder').Process } Process
 * @typedef { import('./rhoBuilder').Name } Name
 * @typedef { import('./rhoBuilder').vdecl } vdecl
 */
export function makeCompiler(bld) {
  const loc = (n) => `${n._pegPosition}`;
  let tmp_ix = 0;
  const fresh = (n) => `${n.type}_${loc(n)}_${tmp_ix++}`;

  function todowarn(expectedType, actual) {
    console.warn(
      `${loc(actual)}: expected ${expectedType}; found ${actual[0]}`
    );
    return bld.primitive(`TODO@@ ${expectedType}` + actual[0]);
  }

  function todoerr(expectedType, actual) {
    return new Error(
      `${loc(actual)}: expected ${expectedType}; found ${JSON.stringify(
        actual
      )}`
    );
  }

  /** @type {(n: string) => Name} */
  const vn = (n) => bld.Var(n);
  const ignore = vn("_"); // TODO: this is a pattern, not a name, no?

  /** @type {(ps: Process[] ) => Process } */
  const par = (ps) => ps.reduce((acc, next) => bld.Par(acc, next));

  /** @param { Expression } js */
  function theLiteral(js) {
    if (js[0] !== "data") {
      throw todoerr("data", js);
    }
    return js[1];
  }

  /**
   *
   * @param {ModuleDeclaration[]} body
   */
  function ofModule(body) {
    console.log({ Program: body.length });
    if (body.length === 0) {
      return bld.Nil();
    }

    /** @type {vdecl[]} */
    const newNames = [];
    /** @type {Declaration} */
    let decl;
    while ((decl = body[0]) && decl[0] === "import") {
      const [_import, [_bind, specifiers], specifier] = decl;
      if (typeof specifier === "string" && specifier.startsWith("rho:")) {
        if (specifiers.length !== 1) {
          throw new Error(`must import default from ${specifier}`);
        }
        newNames.push([specifiers[0][2], specifier]);
      } else {
        const candidates = builtins[specifier] || [];
        if (!candidates.length) {
          todowarn("import from", specifier);
        }
        const unknowns = specifiers
          .map(([_as, name, _alias]) => name)
          .filter((n) => !candidates.includes(n));
        if (unknowns.length > 0) {
          todowarn("import", [unknowns, "from", specifier]);
        }
      }
      body.shift();
    }
    // console.log('new names', newNames);
    if (body.length === 0) {
      return bld.Nil();
    }
    const s0 = body[0];
    /** @type { Binding } */
    let binding;
    if (s0[0] === "exportDefault") {
      /** @type { Binding } */
      binding = ["bind", ["def", "default"], s0[1]];
    } else if (s0[0] === "export") {
      const [_export, _const, bindings] = s0;
      binding = bindings[0];
    }
    const main = bld.new_(
      [...newNames, rhoConsole],
      ofBlock([["const", [binding]]])
    );
    if (body.length > 1) {
      throw todoerr("last");
    }
    console.log("@@@ of module return");
    return main;

    //??? return bld.new_([...newNames, rhoConsole], ofBlock(body));
  }

  /** @param { (Declaration | Statement)[] } js */
  function ofBlock(js) {
    /**
     * @param { (Declaration | Statement)[] } body
     * @returns { Process }
     */
    function recur(body) {
      if (body.length === 0) {
        return bld.Nil();
      }
      const [s0, ...rest] = body;
      switch (s0[0]) {
        case "const":
          const [_const, [decl]] = s0;
          const [_bind, pat, init] = decl;
          switch (init[0]) {
            case "AwaitExpression":
              const { chans, rx, proc } = await2for(decl);
              const restProc = recur(rest);
              return bld.new_(
                chans,
                bld.Par(proc, bld.receiving(rx, restProc))
              );
            case "record":
              if (init[1].length !== 0) {
                throw todoerr("{}", init);
              }
              return bld.new_([pat[1]], recur(rest));
            case "call": {
              const [_, callee, args] = init;
              if (callee[0] !== "use") {
                throw todoerr("use", callee);
              }
              if (callee[1] !== "harden") {
                throw todoerr("harden", callee);
              }
              const { target, methods } = ofMethodSuite(decl);
              return bld.new_([target], par([...methods, recur(rest)]));
            }
            case "data":
              if (pat[0] !== "def") throw todoerr("def", pat);
              const [_def, name] = pat;
              return bld.match(toProc(init), [
                { lhs: bld.Drop(vn(name)), rhs: recur(rest) },
              ]);
            default:
              return bld.Par(
                todowarn(
                  "{} | AwaitExpression | harden({...} | Literal)",
                  decl
                ),
                recur(rest)
              );
          }
        // ExpressionStatement
        case "call":
          try {
            const { arg } = matchConsoleLog(s0);
            // TODO: stdoutack, sequence statements
            return bld.Par(bld.send(vn("console"), [toProc(arg)]), recur(rest));
          } catch (notConsoleLog) {
            const { target, method, args } = matchEventualCall(s0);
            // send only; no return channel
            const procs = args.map(toProc);
            if (method === null) {
              return bld.send(vn(target), procs);
            } else {
              return bld.send(vn(target), [bld.primitive(method), ...procs]);
            }
          }
        case "SwitchStatement":
          const specimen = toProc(s0.discriminant);
          const cases = s0.cases.map(({ test, consequent }) => {
            const lhs = toProc(test);
            const rhs = ofBlock(consequent);
            return { lhs, rhs };
          });
          return bld.Par(bld.match(specimen, cases), recur(rest));
        case "return":
          const retProc = toProc(s0[1] || ["data", null]);
          return bld.send(vn("__return"), [retProc]);
        default:
          return todowarn("Statement | Declaration", s0);
      }
    }
    return recur(js);
  }

  function matchConsoleLog(js) {
    const [call, callee, args] = js;
    const [get, [_use, object], property] = callee;
    if (
      call !== "call" ||
      get !== "get" ||
      object !== "console" ||
      property !== "log" ||
      args.length !== 1
    ) {
      throw todoerr("console.log(...1 arg...)", js);
    }
    const arg = args[0];
    return { arg };
  }

  function theDecl(js) {
    if (js.kind !== "const") {
      throw todoerr("const", js);
    }
    if (js.declarations.length !== 1) {
      throw todoerr("1 declaration", js);
    }
    const decl = js.declarations[0];
    if (decl.type !== "VariableDeclarator") {
      throw todoerr("VariableDeclarator", decl);
    }
    return decl;
  }

  /** @param {Binding} decl */
  function ofMethodSuite(decl) {
    const [_bind, [_def, target], init] = decl;
    const [_call, _harden, args] = init;
    if (args.length !== 1) {
      throw todoerr("harden(1 arg)", init);
    }
    const objectLiteral = args[0];
    if (objectLiteral[0] !== "record") {
      throw todoerr("record", objectLiteral);
    }
    const returnChans = (params) =>
      params.map(([_def, name]) => name).includes("__return")
        ? []
        : [vn("__return")];
    const methods = objectLiteral[1].map((prop) => {
      if (prop[0] !== "method") throw todoerr("method", prop);
      const [_method, name, params, [_block, body]] = prop;
      return bld.contract(
        vn(target),
        [
          bld.Quote(bld.primitive(name)),
          ...params.map(([_def, name]) => vn(name)),
          ...returnChans(params),
        ],
        ofBlock(body)
      );
    });
    return { target, methods };
  }

  function await2for(decl) {
    const rhs = decl.init;
    if (rhs.type !== "AwaitExpression") {
      throw todoerr("AwaitExpression", rhs);
    }
    const expr = rhs.argument;
    if (
      expr.type === "CallExpression" &&
      expr.callee.type === "MemberExpression" &&
      expr.callee.object.name === "Promise"
    ) {
      const { chans, proc } = ofPromiseAll(rhs.argument);
      const patts = ofArrayPattern(decl.id);
      if (chans.length !== patts.length) {
        throw new Error(
          `Promise.all mismatch: ${chans.length} expressions to ${patts.length} channels`
        );
      }
      const rx = patts.map((lhs, ix) => ({ lhs: [lhs], rhs: vn(chans[ix]) }));
      return { chans, proc, rx };
    } else {
      const chan = fresh(rhs);
      const proc = ofEventualCall(expr, chan);
      const lhs = ofPattern(decl.id);
      return { chans: [chan], proc, rx: [{ lhs: [lhs], rhs: vn(chan) }] };
    }
  }

  /**
   * @param { Pattern } js
   * @returns {Name}
   */
  function ofPattern(js) {
    switch (js[0]) {
      case "def":
        return vn(js[1]);
      case "ObjectPattern":
        const unk = js.properties.filter(
          ({ key }, ix) => key.name !== `_${ix}`
        );
        if (unk.length > 0) {
          throw todoerr("tuple pattern { _0: ..., _1: ..., _2: ..., ... ", js);
        }
        const parts = js.properties.map(({ value }) => toProc(value));
        return bld.Quote(bld.tupleExpr(parts));
      default:
        throw todoerr("Identifier | ObjectPattern", js);
    }
  }
  /**
   * @returns {Name[]}
   */
  function ofArrayPattern(js) {
    if (js.type !== "ArrayPattern") {
      throw todoerr("ArrayPattern", js);
    }
    return js.elements.map(ofPattern);
  }

  /**
   * Make Par from await Promise.all
   * @returns { { chans: string[], proc: Process }}
   */
  function ofPromiseAll(js) {
    if (js.type !== "CallExpression") {
      throw todoerr("CallExpression", js);
    }
    if (
      js.callee.type !== "MemberExpression" ||
      js.callee.object.name !== "Promise" ||
      js.callee.property.name !== "all"
    ) {
      throw todoerr("Promise.all", js.callee);
    }
    if (
      js.arguments.length !== 1 ||
      js.arguments[0].type !== "ArrayExpression"
    ) {
      throw todoerr("ArrayExpression[1]", js.arguments);
    }
    const items = js.arguments[0].elements;
    const chans = items.map(fresh);
    const procs = items.map((expr, ix) => ofEventualCall(expr, chans[ix]));
    return { chans, proc: par(procs) };
  }

  function ofEventualCall(js, k) {
    const { target, method, args } = matchEventualCall(js);
    const procs = [...args.map(toProc), bld.Drop(vn(k))];
    if (method === null) {
      return bld.send(vn(target), procs);
    } else {
      return bld.send(vn(target), [bld.primitive(method), ...procs]);
    }
  }
  /** @param { Expression } js */
  function matchEventualCall(js) {
    if (js[0] !== "call") {
      throw todoerr("call", js);
    }
    const [_call, callee, args] = js;
    const { target, method } = matchEventualTarget(callee);
    return { target, method, args };
  }
  /** @param { Expression } js */
  function matchEventualTarget(js) {
    switch (js[0]) {
      case "call":
        return { target: matchE(js), method: null };
      case "get":
        const [_get, object, name] = js;
        return { target: matchE(object), method: name };
      default:
        throw todoerr("call | get", js);
    }
  }
  /** @param { Expression } js */
  function matchE(js) {
    if (js[0] !== "call" || js[1][0] !== "use" || js[1][1] !== "E") {
      throw todoerr("E(...)", js);
    }
    if (js[2].length != 1) {
      throw todoerr("E(_1 arg_)", js);
    }
    const target = js[2][0];
    if (target[0] !== "use") {
      throw todoerr("use", target);
    }
    return target[1];
  }

  /**
   * @param { Expression } js
   * @returns { Process }
   */
  function toProc(js) {
    switch (js[0]) {
      case "data":
        const [_t, v] = js;
        if (typeof v == "object") {
          if (v === null) {
            return bld.Nil();
          }
          throw typeof v; // TODO: RegExp?
        }
        return bld.primitive(v);
      case "use":
        return bld.Drop(vn(js[1]));
      case "LogicalExpression":
      case "BinaryExpression":
        let op = { "&&": "and", "||": "or", "===": "==" }[js.operator];
        if (!op) {
          throw todoerr("&& || ===", js);
        }
        return bld.binop(op, toProc(js.left), toProc(js.right));
      case "call":
        const [_call, callee, args] = js;
        if (callee[0] !== "use") {
          throw todoerr("use", callee);
        }
        const arg = args[0];
        switch (callee[1]) {
          case "bundlePlus":
            if (args.length !== 1) {
              throw todoerr("BUILTIN(...1 arg...)");
            }
            return bld.unary("bundle+", toProc(arg));
          case "tuple":
            return bld.tupleExpr(args.map(toProc));
          default:
            throw todoerr("bundlePlus", callee[1]);
        }
      case "ObjectExpression":
        const entries = js.properties.map(({ key, value }) =>
          // TODO: theLiteral
          ({ key: key.value, value: toProc(value) })
        );
        return bld.mapExpr(entries);
      default:
        throw todoerr("Literal | Identifier", js);
    }
  }

  /** @type {(js: Module) => Process} */
  function compiler(js) {
    // console.log("DEBUG: node:", JSON.stringify(js));
    console.log("DEBUG: js.type", js[0]);

    switch (js[0]) {
      case "ExpressionStatement":
        return toProc(js.expression);

      case "BlockStatement":
        if (js.body.length == 1) {
          return js2rho(js.body[0]);
        }
        return ofBlock(js.body);
      case "module":
        return ofModule(js[1]);
      default:
        return todowarn(
          "Literal | ExpressionStatement | BlockStatemnt | Program",
          js
        );
    }
  }
  return compiler;
}

/**
 *
 * @param {string} src
 * @param {Writer} out
 * @param {{ sourceURL: string }} options
 *
 * @typedef { import('./rhoBuilder').Writer } Writer
 */
export async function js2rho(src, out, { sourceURL }) {
  const bld = rhoBuilder();
  const compiler = makeCompiler(bld);

  let indent = 0;
  const printer = harden({
    /** @param { string } txt */
    print(txt) {
      out.write(txt);
    },
    /** @param { string } txt */
    begin(txt) {
      out.write(txt);
      indent += 1;
      printer.newline();
    },
    newline() {
      out.write("\n" + "  ".repeat(indent));
    },
    /** @param { string } txt */
    end(txt) {
      printer.newline();
      out.write(txt);
      indent -= 1;
      printer.newline();
    },
  });
  const prog = await parse(src, { sourceURL });
  // console.log(JSON.stringify(prog, null, 2));
  compiler(prog)._printOn(printer);
}

const helloWorld = `
import { harden } from "@agoric/harden";

const world = harden({
  hello() {
    console.log('Hello, World!');
  },
});
`;

/**
 * @param { string[] } argv
 * @param {{
 *   readFile: typeof import('fs').promises.readFile,
 *   stdout: typeof process.stdout
 * }} io
 */
async function main(argv, { readFile, stdout }) {
  const [sourceURL] = argv.slice(2);
  const src = sourceURL ? await readFile(sourceURL, "utf-8") : helloWorld;
  js2rho(src, { write: (txt) => stdout.write(txt) }, { sourceURL });
}

if (require.main === module) {
  main([...process.argv], {
    readFile: require("fs").promises.readFile,
    stdout: process.stdout,
  }).catch((err) => {
    console.error(err);
    throw err;
  });
}
