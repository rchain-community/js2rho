// @ts-check
/// <reference path="node_modules/@types/node/index.d.ts" />
/// <reference path="node_modules/@types/esprima/index.d.ts" />
import esprima from "esprima";
const { parseModule, Program } = esprima;
//import { Node } from "estree";

import poolSrc from './testSuite.js';

// console.log(process.argv); TODO: get filename from process.argv


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
 * @typedef {string} vdecl
 */
     

/**
 * @typedef {Object} RhoBuilder is a thingy
 * @property {() => Process} Nil
 * @property {(v: boolean | number | string) => Process} primitive
 * @property {(dest: Name, procs: Array<Process>) => Process} send
 * @property {(op: BinOp, lhs: Process, rhs: Process) => Process} binop
 * @property {(lhs: Array<Name>, rhs: Name, proc: Process) => Process} receiving
 * @property {(name: Name, args: Array<Name>, body: Process) => Process} contract
 * @property {(n: Name) => Process} Drop
 * @property {(p: Process, q: Process) => Process} Par
 * @property {(id: string) => Name} Var
 * @property {(p: Process) => Name} Quote
 * @property {(vars: Array<vdecl>, body: Process) => Process} new_
 */

 /**
  * @returns {RhoBuilder}
  */
function rhoBuilder() {
    const Nil = () => Object.freeze({
        _printOn: (out) => out.print("Nil"),
        quote: () => Quote(Nil())
    });

    const primitive = (v) => Object.freeze({
        _printOn: (out) => out.print(JSON.stringify(v)),
        quote: () => Quote(primitive(v))
    });
    const Quote = (p) => Object.freeze({
        _printOn: (out) => {
            out.print("@{ ");
            p._printOn(out)
            out.print(" }")
        },
        deref: () => p
    });
    const Drop = (name) => Object.freeze({
        _printOn: (out) => {
            out.print("*");
            name._printOn(out)
        },
        quote: () => name
    });
    const printList = (out, items) => {
        let first = true;
        for (let item of items) {
            if (!first) {
                out.print(", ")
            }
            item._printOn(out)
            first = false
        }
    }
    const send = (dest, procs) => Object.freeze({
        _printOn: (out) => {
            dest._printOn(out)
            out.print(`!(`)  // TODO: !!
            printList(out, procs)
            out.print(")")
        },
        quote: () => Quote(send(dest, procs))
    });
    const binop = (op, lhs, rhs) => Object.freeze({
        _printOn: (out) => {
            lhs._printOn(out)
            out.print(" " + op + " ")
            rhs._printOn(out)
        },
        quote: () => Quote(binop(op, lhs, rhs))
    })
    const receiving = (lhs, rhs, proc) => Object.freeze({
        _printOn: (out) => {
            out.print("for(")
            printList(out, lhs)
            out.print(` <- `)
            rhs._printOn(out)
            out.print(`) {\n `)
            proc._printOn(out)
            out.print("\n}\n")
        },
        quote: () => Quote(receiving(lhs, rhs, proc))
    })
    const contract = (name, args, body) => Object.freeze({
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
    const Par = (p, q) => Object.freeze({
        _printOn: (out) => {
            p._printOn(out);
            out.print(" | ");
            q._printOn(out);
        },
        quote: () => Quote(Par(p, q))
    });
    const Var = (v) => Object.freeze({
        _printOn: (out) => out.print(v),
        deref: () => Drop(Var(v))
    })
    const new_ = (vlist, body) => Object.freeze({
        _printOn: (out) => {
            out.print("new ")
            out.print(vlist.join(", "))
            out.print(" in {\n")
            body._printOn(out)
            out.print("\n}\n")
        },
        quote: () => Quote(new_(vlist, body))
    })

    return Object.freeze({
        Nil,
        primitive,
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

/**
 * 
 * @param {RhoBuilder} bld 
 */
function makeCompiler(bld) {
    let tmp_ix = 0;
    const fresh = (n) => `l${n.loc.start.line}c${n.loc.start.column}_${tmp_ix++}`

    /** @type {(n: string) => Name} */    
    const vn = (n) => bld.Var(n)
    const ignore = vn("_")  // TODO: this is a pattern, not a name, no?

    /** @type {(ps: Process[] ) => Process } */
    const par = (ps) => ps.reduce((acc, next) => bld.Par(acc, next));

    /** @type {(js: Node, k: Name) => Process} */
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
                console.log("@@VariableDeclaration not impl:", JSON.stringify(js, null, 2))
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
                console.log("BlockStatement @@not impl:", JSON.stringify(js, null, 2))
                return bld.primitive(js.type)
            case "Program":
                function build(body) {
                    if (body.length === 0) {
                        return bld.Nil;
                    }
                    if (body[0].type === 'ImportDeclaration') {
                        const decl = body[0];
                        const p = build(body.slice(1));
                        if (decl.source.type !== 'Literal') {
                            throw new Error(decl.source.type);
                        }
                        if (decl.source.value === '@rchain-community/js2rho') {
                            for (const spec of decl.specifiers) {
                                if (!['bundlePlus', 'tuple'].includes(spec.local.name)) {
                                    throw new Error(`unrecognized name ${spec.local.name} from @rchain-community/js2rho`);
                                }
                                // TODO: symbol table? "live" bindings?
                            }
                            return p;
                        } else if (typeof decl.source.value === 'string' && decl.source.value.startsWith('rho:')) {
                            if (decl.specifiers.length !== 1) {
                                throw new Error(`must import 1 name from ${decl.source.value}`);
                            }
                            const spec = decl.specifiers[0];
                            // TODO: new x(`uuu`)
                            return bld.new_([spec.local.name], p);  // TODO: optimize new x { new y { ... } } to new x, y { ... }
                        }
                    } else {
                        throw new Error(`TODO: ${body[0].type} in Program: ${JSON.stringify(body[0])}`);
                    }
                }
                return build(js.body);
            default:
                console.log("@@not impl:", JSON.stringify(js, null, 2))
                return bld.primitive(js.type)
        }
    }
    return js2rho;
}

const tests = [
    {
        src: poolSrc,
    },
    {
        src: `
    m.set(purse, decr);
    `,
    },
    {
        src: `
    const decr = amount => {
        balance = Nat(balance - amount);
    };
    `,
    },
    {
        src: `
const makeMint = () => {
    const m = WeakMap();
    const makePurse = () => mint(0);
    const mint = balance => {
        const purse = def({
            getBalance: () => balance,
            makePurse: makePurse,
            deposit:
                (amount, srcP) => Q(srcP).then(src => {
                    Nat(balance + amount);
                    m.get(src)(Nat(amount));
                    balance += amount;
                })
        });
        const decr = amount => {
            balance = Nat(balance - amount);
        };
        m.set(purse, decr);
        return purse;
    };
    return mint;
};
`,
    },
];

export default
function unittest(out) {
    const bld = rhoBuilder();
    const compiler = makeCompiler(bld);
    const printer = Object.freeze({
        print: (txt) => out.write(txt)
    })

    for (const item of tests) {
        console.log('\n==== JS SOURCE CODE ====\n', item.src);
        const prog = parseModule(item.src, { loc: true });

        console.log('==== AST ====\n', JSON.stringify(prog, null, 2));

        console.log('==== RHO ====\n');
        compiler(prog, bld.Var("top"))._printOn(printer);
    }
}

unittest(process.stdout);
