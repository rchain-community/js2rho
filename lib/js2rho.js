// @ts-check
import harden from "@agoric/harden";
import { parse } from "../../lib/translate";
import { rhoBuilder } from "./rhoBuilder";

export const builtins = {
  "@rchain-community/js2rho": ["bundlePlus", "tuple", "console"],
  "@agoric/nat": ["Nat"],
  "@agoric/harden": ["harden"],
  "@agoric/eventual-send": ["E"],
};

/** type: {vdecl} */
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
  const loc = (n) => "@@TODOloc"; // `${n.loc.start.line}c${n.loc.start.column}`;
  let tmp_ix = 0;
  const fresh = (n) => `${n.type}_${loc(n)}_${tmp_ix++}`;

  function todowarn(expectedType, actual) {
    console.warn(
      `${loc(actual)}: expected ${expectedType}; found ${actual[0]}`
    );
    return bld.primitive("TODO@@ " + actual[0]);
  }

  function todoerr(expectedType, actual) {
    return new Error(
      `${loc(actual)}: expected ${expectedType}; found ${
        actual ? actual[0] : actual
      }`
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
        const candidates = builtins[specifier];
        if (!candidates) {
          throw new Error(`not supported: import ... from ${specifier}`);
        }
        const unknowns = specifiers
          .map(([_as, _g, local]) => local)
          .filter((n) => !candidates.includes(n));
        if (unknowns.length > 0) {
          throw new Error(`unrecognized name(s) ${unknowns} from ${specifier}`);
        }
      }
      body.shift();
    }
    // console.log('new names', newNames);
    if (body.length === 0) {
      return bld.Nil();
    }
    const s0 = body[0];
    if (s0.type === "ExportDefaultDeclaration") {
      const fn0 = s0.declaration;
      if (fn0.type !== "FunctionDeclaration") {
        throw todoerr("FunctionDeclaration", fn0);
      }
      const main = bld.new_([...newNames, rhoConsole], ofBlock(fn0.body.body));
      if (body.length > 1) {
        throw todoerr("last", fn0);
      }
      return main;
    }
    return bld.new_([...newNames, rhoConsole], ofBlock(body));
  }

  /** @param { (Declaration | Statement)[] } js */
  function ofBlock(js) {
    /** @param { (Declaration | Statement)[] } body */
    function recur(body) {
      if (body.length === 0) {
        return bld.Nil();
      }
      const [s0, ...rest] = body;
      switch (s0[0]) {
        case "const":
          const [_const, [decl]] = s0;
          const [_bind, pat, expr] = decl;
          switch (expr[0]) {
            case "AwaitExpression":
              const { chans, rx, proc } = await2for(decl);
              const restProc = recur(rest);
              return bld.new_(
                chans,
                bld.Par(proc, bld.receiving(rx, restProc))
              );
            case "ObjectExpression":
              if (decl.init.properties.length !== 0) {
                throw todoerr("{}", decl.init);
              }
              return bld.new_([decl.id.name], recur(rest));
            case "call": {
              const [_, callee, args] = expr;
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
              return bld.match(toProc(expr), [
                { lhs: bld.Drop(vn(decl.id.name)), rhs: recur(rest) },
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
        case "call":
          try {
            const { arg } = matchConsoleLog(s0);
            // TODO: stdoutack, sequence statements
            return bld.Par(bld.send(vn("console"), [toProc(arg)]), recur(rest));
          } catch (notConsoleLog) {
            const { target, method, args } = matchEventualCall(s0.expression);
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
        case "ReturnStatement":
          const retProc = toProc(s0.argument);
          return bld.send(vn("__return"), [retProc]);
        default:
          return todowarn("VariableDeclaration", s0);
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

  function ofMethodSuite(decl) {
    const [_bind, [_def, target], rhs] = decl;
    const [_call, _harden, args] = rhs;
    if (args.length !== 1) {
      throw todoerr("harden(1 arg)", decl.init);
    }
    const objectLiteral = args[0];
    if (objectLiteral[0] !== "record") {
      throw todoerr("record", objectLiteral);
    }
    const returnChans = (params) =>
      params.map(([_def, name]) => name).includes("__return")
        ? []
        : [vn("__return")];
    const methods = objectLiteral[1].map(
      ([_method, name, params, [_block, body]]) =>
        bld.contract(
          vn(target),
          [
            bld.Quote(bld.primitive(name)),
            ...params.map(([_def, name]) => vn(name)),
            ...returnChans(params),
          ],
          ofBlock(body)
        )
    );
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
  function matchEventualCall(js) {
    if (js.type !== "CallExpression") {
      throw todoerr("CallExpression", js);
    }
    const { target, method } = matchEventualTarget(js.callee);
    return { target, method, args: js.arguments };
  }
  function matchEventualTarget(js) {
    switch (js.type) {
      case "CallExpression":
        return { target: matchE(js), method: null };
      case "MemberExpression":
        return { target: matchE(js.object), method: js.property.name };
      default:
        throw todoerr("CallExpression | MemberExpression", js);
    }
  }
  function matchE(js) {
    if (js.callee.name !== "E") {
      throw todoerr("E(...)", js);
    }
    if (js.arguments.length != 1) {
      throw todoerr("E(_1 arg_)", js);
    }
    const target = js.arguments[0];
    if (target.type !== "Identifier") {
      throw todoerr("Identifier", target);
    }
    return target.name;
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
      case "CallExpression":
        if (js.callee.type !== "Identifier") {
          throw todoerr("Identifier", js.callee);
        }
        const arg = js.arguments[0];
        switch (js.callee.name) {
          case "bundlePlus":
            if (js.arguments.length !== 1) {
              throw todoerr("BUILTIN(...1 arg...)");
            }
            return bld.unary("bundle+", toProc(arg));
          case "tuple":
            return bld.tupleExpr(js.arguments.map(toProc));
          default:
            throw todoerr("bundlePlus", js);
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
 *
 * @typedef { import('./rhoBuilder').Writer } Writer
 */
export async function js2rho(src, out) {
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
  const prog = await parse(src, { sourceURL: "somewhere" });
  // console.log(JSON.stringify(prog, null, 2));
  compiler(prog)._printOn(printer);
}

async function main({ stdout }) {
  const src = `
  import { harden } from "@agoric/harden";

  const world = harden({
    hello() {
      console.log('Hello, World!');
    },
  });
  `;
  js2rho(src, { write: (txt) => stdout.write(txt) });
}

if (require.main === module) {
  main({ stdout: process.stdout }).catch((err) => {
    console.error(err);
    throw err;
  });
}
