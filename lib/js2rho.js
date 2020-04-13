// @ts-check
import esprima from "esprima";
const { parseModule, Program } = esprima;

import poolSrc from '../test/testSuite.js';

const { freeze: harden } = Object;

// console.log(process.argv); TODO: get filename from process.argv


/**
 *
 * @param {string} src
 * @param {Writer} out
 */
export function js2rho(src, out) {
    const bld = rhoBuilder();
    const compiler = makeCompiler(bld);
    const printer = harden({
        print: (txt) => out.write(txt)
    });
    const prog = parseModule(src, { loc: true });
    compiler(prog, bld.Var("top"))._printOn(printer);
}


/**
 * @typedef {Object} Miranda
 * @property {(out: Printer) => void} _printOn
 *
 * @typedef {Object} Printer
 * @property {(s: string) => void} print
 *
 * @typedef {Miranda} Process
 * @property {() => Name} quote
 *
 * @typedef {Object} Name
 * @property {(out: Printer) => void} _printOn
 * @property {() => Process} deref
 */

/**
 * @typedef {"==" | "!="| "and" | "or"| "+"| "*" | "%"| "-" | "/"| "<=" | "<" | ">=" | ">"| "bitand" | "bitor"} BinOp
    // Structurally equivalent processes: "==" | "!="
    // Boolean operators: "and" | "or"
    // Integers, strings, and datetimes: "+"
    // Integers and strings: "*" | "%"
    // Integers: "-" | "/"
    // All ordered types: "<=" | "<" | ">=" | ">"
    // Bitfields: "bitand" | "bitor"
 *
 * // TODO: types, iopairs
 * @typedef {string | [string, string]} vdecl
 */


/**
 * https://github.com/rchain/rchain/blob/dev/rholang/src/main/bnfc/rholang_mercury.cf
 *
 * @typedef {Object} RhoBuilder is a thingy
 * @property {() => Process} Nil
 * @property {(v: boolean | number | string) => Process} primitive
 * @property {(procs: Process[]) => Process} listExpr
 * @property {(dest: Name, procs: Array<Process>) => Process} send
 * @property {(op: BinOp, lhs: Process, rhs: Process) => Process} binop
 * @property {(rx: Receipt, proc: Process) => Process} receiving
 * @property {(name: Name, args: Array<Name>, body: Process) => Process} contract
 * @property {(n: Name) => Process} Drop
 * @property {(p: Process, q: Process) => Process} Par
 * @property {(id: string) => Name} Var
 * @property {(p: Process) => Name} Quote
 * @property {(vars: Array<vdecl>, body: Process) => Process} new_
 *
 * @typedef {{ lhs: Name[], rhs: Name}[]} Receipt
 */

/**
 * @returns {RhoBuilder}
 */
export function rhoBuilder() {
    const Nil = () => harden({
        _printOn: (out) => out.print("Nil"),
        quote: () => Quote(Nil())
    });

    const primitive = (v) => harden({
        _printOn: (out) => out.print(JSON.stringify(v)),
        quote: () => Quote(primitive(v))
    });
    const listExpr = (procs) => harden({
        _printOn(out) {
            out.print('[');
            printList(out, procs);
            out.print(']');
        },
        quote: () => Quote(listExpr(procs))
    });
    const Quote = (p) => harden({
        _printOn: (out) => {
            out.print("@{ ");
            p._printOn(out)
            out.print(" }")
        },
        deref: () => p
    });
    const Drop = (name) => harden({
        _printOn: (out) => {
            out.print("*");
            name._printOn(out)
        },
        quote: () => name
    });
    const printList = (out, items) => {
        let first = true;
        for (const item of items) {
            if (!first) {
                out.print(", ")
            }
            item._printOn(out)
            first = false
        }
    }
    const send = (dest, procs) => harden({
        _printOn: (out) => {
            dest._printOn(out)
            out.print(`!(`)  // TODO: !!
            printList(out, procs)
            out.print(")")
        },
        quote: () => Quote(send(dest, procs))
    });
    const binop = (op, lhs, rhs) => harden({
        _printOn: (out) => {
            lhs._printOn(out)
            out.print(" " + op + " ")
            rhs._printOn(out)
        },
        quote: () => Quote(binop(op, lhs, rhs))
    })
    const receiving = (rx, proc) => harden({
        _printOn: (out) => {
            out.print("for(")
            let first = true;
            for (const { lhs, rhs } of rx) {
                if (!first) {
                    out.print(";\n  ")
                }
                printList(out, lhs)
                out.print(` <- `)
                rhs._printOn(out)
                first = false;
            }
            out.print(`) {\n `)
            proc._printOn(out)
            out.print("\n}\n")
        },
        quote: () => Quote(receiving(rx, proc))
    })
    const contract = (name, args, body) => harden({
        _printOn: (out) => {
            out.print("contract ")
            name._printOn(out)
            out.print(`(`)
            printList(out, args)
            out.print(`) = {\n`)
            body._printOn(out)
            out.print("\n}\n")
        },
        quote: () => Quote(contract(name, args, body))
    })
    const Par = (p, q) => harden({
        _printOn: (out) => {
            p._printOn(out);
            out.print(" | ");
            q._printOn(out);
        },
        quote: () => Quote(Par(p, q))
    });
    const Var = (v) => harden({
        _printOn: (out) => out.print(v),
        deref: () => Drop(Var(v))
    })

    const fmtvdecl = (vd) => typeof vd === 'string' ? vd : `${vd[0]}(\`${vd[1]}\`)`;
    /** @type {(vlist: vdecl[], body: Process) => Process} */
    const new_ = (vlist, body) => harden({
        _printOn: (out) => {
            out.print("\nnew ")
            out.print(vlist.map(fmtvdecl).join(",\n  "))
            out.print("\nin {\n")
            body._printOn(out)
            out.print("\n}\n")
        },
        quote: () => Quote(new_(vlist, body))
    })

    return harden({
        Nil,
        primitive,
        listExpr,
        send,
        binop,
        receiving,
        contract,
        Drop,
        Var,
        Quote,
        Par,
        new_,
    });
}

export const builtins = {
    '@rchain-community/js2rho': ['bundlePlus', 'tuple', 'console'],
    '@agoric/eventual-send': ['E'],
};
const rhoConsole = ['console', 'rho:io:stdout'];


function jsontrim(obj, maxlen = 80) {
    return JSON.stringify(obj).slice(0, maxlen);
}

//throw new Error(`${loc(defaultExport)}: expected default export; found ${defaultExport.type}`);


/**
 *
 * @param {RhoBuilder} bld
 * @returns {(js: Program, k: Name) => Process}
 */
export function makeCompiler(bld) {
    const loc = (n) => `${n.loc.start.line}c${n.loc.start.column}`
    let tmp_ix = 0;
    const fresh = (n) => `l${loc(n)}_${tmp_ix++}`

    function todowarn(expectedType, actual) {
        console.warn(`${loc(actual)}: expected ${expectedType}; found ${actual.type}`);
        return bld.primitive(actual.type);
    }

    function todoerr(expectedType, actual) {
        return new Error(`${loc(actual)}: expected ${expectedType}; found ${actual.type}`);
    }

    /** @type {(n: string) => Name} */
    const vn = (n) => bld.Var(n)
    const ignore = vn("_")  // TODO: this is a pattern, not a name, no?

    /** @type {(ps: Process[] ) => Process } */
    const par = (ps) => ps.reduce((acc, next) => bld.Par(acc, next));

    /**
     *
     * @param {Node[]} body
     */
    function ofModule(body) {
        console.log({ Program: body.length });
        if (body.length === 0) {
            return bld.Nil();
        }
        const defaultExport = body[body.length - 1];
        if (defaultExport.type !== 'ExportDefaultDeclaration') {
            return todowarn('ExportDefaultDeclaration', defaultExport);
        }

        /** @type {vdecl[]} */
        const newNames = body.slice(0, body.length - 1).flatMap(decl => {
            if (decl.type !== 'ImportDeclaration') {
                throw todoerr('ImportDeclaration', decl);
            }
            if (decl.source.type !== 'Literal') {
                throw todoerr('Literal', decl.source);
            }
            const specifier = decl.source.value;
            if (typeof specifier === 'string' && specifier.startsWith('rho:')) {
                if (decl.specifiers.length !== 1) {
                    throw new Error(`must import 1 name from ${specifier}`);
                }
                return [[decl.specifiers[0].local.name, specifier]];
            } else {
                const candidates = builtins[specifier];
                if (!candidates) {
                    throw new Error(`not supported: import ... from ${specifier}`);
                }
                const unknowns = decl.specifiers.map(s => s.local.name).filter(n => !candidates.includes(n));
                if (unknowns.length > 0) {
                    throw new Error(`unrecognized name(s) ${unknowns} from ${specifier}`);
                }
                return [];
            }
        });
        // console.log('new names', newNames);
        const fn0 = defaultExport.declaration;
        if (fn0.type !== 'FunctionDeclaration') {
            throw todoerr('FunctionDeclaration', fn0);
        }
        return bld.new_(newNames.concat([rhoConsole]), ofBlock(fn0.body));
    }

    function ofBlock(js) {
        if (js.type !== 'BlockStatement') {
            throw todoerr('BlockStatement', js);
        }
        const body = js.body;
        if (body[0].type !== 'VariableDeclaration') {
            throw todoerr('VariableDeclaration', body[0]);
        }
        // TODO: body[1...]
        return await2for(body[0]);
    }

    function await2for(js) {
        if (js.kind !== 'const') {
            throw new Error(jsontrim(js))
        }
        if (js.declarations.length !== 1) {
            throw new Error(jsontrim(js))
        }
        const decl = js.declarations[0];
        if (decl.type !== "VariableDeclarator") {
            throw todoerr('VariableDeclarator', decl);
        }
        const rhs = decl.init;
        if (rhs.type !== 'AwaitExpression') {
            throw todoerr('AwaitExpression', rhs);
        }
        const { chans, proc } = ofPromiseAll(rhs.argument);
        const patts = ofArrayPattern(decl.id);
        if (chans.length !== patts.length) {
            throw new Error(`Promise.all mismatch: ${chans.length} expressions to ${patts.length} channels`);
        }
        const rx = patts.map((lhs, ix) => ({ lhs: [lhs], rhs: vn(chans[ix]) }));
        return bld.new_(chans, bld.receiving(rx, proc));
    }

    /**
     * @returns {Name[]}
     */
    function ofArrayPattern(js) {
        if (js.type !== "ArrayPattern") {
            throw todoerr('ArrayPattern', js);
        }
        return js.elements.map(elt => {
            if (elt.type !== 'Identifier') {
                throw todoerr('Identifier', elt);
            }
            return vn(elt.name);
        });
    }

    /**
     * Make Par from await Promise.all
     * @returns { { chans: string[], proc: Process }}
     */
    function ofPromiseAll(js) {
        if (js.type !== "CallExpression") {
            throw todoerr('CallExpression', js);
        }
        if (js.callee.type !== 'MemberExpression' ||
            js.callee.object.name !== 'Promise' ||
            js.callee.property.name !== 'all') {
            throw todoerr('Promise.all', js.callee);
        }
        if (js.arguments.length !== 1 ||
            js.arguments[0].type !== 'ArrayExpression') {
            throw todoerr('ArrayExpression[1]', js.arguments);
        }
        const items = js.arguments[0].elements;
        const chans = items.map(fresh);
        const procs = items.map((expr, ix) => js2rho(expr, chans[ix]));
        return { chans, proc: par(procs) };
    }

    /** @type {(js: Program, k: Name) => Process} */
    function js2rho(js, k) {
        // console.log("DEBUG: node:", JSON.stringify(js));
        console.log("DEBUG: js.type", js.type);

        const recur = (js) => {
            const kr = fresh(js)
            return { v: kr, p: js2rho(js, vn(kr)) }
        }
        const kont = (proc) => bld.send(k, [proc]);
        const patname = (pat) => {
            if (pat.type != "Identifier") {
                throw (pat)  // TODO: other param patterns
            }
            return pat.name
        }

        switch (js.type) {
            case "Literal":
                const v = js.value
                if (typeof (v) == "object") {
                    throw (v)  // TODO: RegExp
                }
                return kont(bld.primitive(v))
            case "Identifier":
                // Assume each js ident refers to a cell
                return bld.send(vn(js.name),
                    [bld.primitive("get"),
                    bld.Drop(k)])
            case "BinaryExpression":
                const op = js.operator
                if (op == "===" || op == "!==" ||
                    op == "<<" || op == ">>" ||
                    op == ">>>" ||
                    op == "**" ||
                    op == "|" || op == "&" ||
                    op == "^" ||
                    op == "in" ||
                    op == "instanceof") {
                    throw (op) // TODO
                }
                // We assume order of arguments doesn't matter.
                const lt = recur(js.left),
                    rt = recur(js.right)
                return bld.new_([lt.v, rt.v], par([lt.p, rt.p,
                bld.send(k, [bld.binop(op, vn(lt.v).deref(), vn(rt.v).deref())])]))
            case "CallExpression":
                const args = js.arguments.map(recur)
                let target;
                let verb = []
                /* special case for obj.prop(...) */
                if (js.callee.type == "MemberExpression" &&
                    js.callee.property.type == "Identifier") {
                    target = recur(js.callee.object)
                    const prop = js.callee.property
                    verb = [bld.primitive(prop.name)]
                } else {
                    target = recur(js.callee)
                }
                const vs = [].concat([target.v], args.map(a => a.v));
                const call = bld.send(vn(target.v), [].concat(verb, args.map(a => vn(a.v).deref()), [k]))
                return bld.new_(vs,
                    par([].concat(args.map(a => a.p), [target.p, call])))
            case "ExpressionStatement":
                return js2rho(js.expression, k);
            case "VariableDeclaration":
                const decls = js.declarations
                /* special case for const f = (a, b, c, ...) => e */
                if (js.kind == "const" && decls.length == 1) {
                    const decl = decls[0]
                    if (decl.type == "VariableDeclarator") {
                        const id = decl.id
                        if (id.type == "Identifier") {
                            const lhs = bld.Var(id.name)
                            const init = decl.init
                            if (init.type == "ArrowFunctionExpression") {
                                const ret = fresh(init)
                                const params = [].concat(
                                    init.params.length == 0 ?
                                        [ignore] : (
                                            init.params.map(pat => vn(patname(pat)))),
                                    [vn(ret)])
                                const contract = bld.contract(lhs, params, js2rho(init.body, vn(ret)))
                                const done = bld.send(k, [bld.Nil()])
                                return bld.Par(contract, done)
                            }
                        }
                    }
                }
                console.log("@@VariableDeclaration not impl:", JSON.trim(js))
                return bld.primitive(js.type)
            case "AssignmentExpression":
                const lhs = patname(js.left)
                const rhs = recur(js.right)
                const set = bld.send(vn(lhs), [bld.primitive("set"), vn(rhs.v).deref(), k.deref()])
                return bld.new_([rhs.v], par([rhs.p, set]))
            case "BlockStatement":
                if (js.body.length == 1) {
                    return js2rho(js.body[0], k)
                }
                console.log("BlockStatement @@not impl:", JSON.stringify(js).slice(0, 480))
                return bld.primitive(js.type)
            case "Program":
                return ofModule(js.body);
            default:
                console.log("@@not impl:", JSON.stringify(js, null, 2))
                return bld.primitive(js.type)
        }
    }
    return js2rho;
}
