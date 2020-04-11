/// <reference path="node_modules/@types/node/index.d.ts" />
/// <reference path="node_modules/@types/esprima/index.d.ts" />
import { parseScript, Program } from "esprima";
import { Node } from "estree";

// console.log(process.argv); TODO: get filename from process.argv


interface Miranda {
    _printOn(out: Printer)
}
interface Printer {
    print(s: string)
}
interface Process extends Miranda {
    quote(): Name
}
interface Name extends Miranda {
    deref(): Process
}
type BinOp = (
    // Structurally equivalent processes
    "==" | "!="
    // Boolean operators
    | "and" | "or"
    // Integers, strings, and datetimes
    | "+"
    // Integers and strings
    | "*" | "%"
    // Integers
    | "-" | "/"
    // All ordered types
    | "<=" | "<" | ">=" | ">"
    // Bitfields
    | "bitand" | "bitor"
)

type vdecl = string; // TODO: types, iopairs

interface RhoBuilder {
    Nil(): Process
    primitive(v: boolean | number | string): Process
    send(dest: Name, procs: Array<Process>): Process
    binop(op: BinOp, lhs: Process, rhs: Process): Process
    receiving(lhs: Array<Name>, rhs: Name, proc: Process): Process
    contract(name: Name, args: Array<Name>, body: Process): Process
    Drop(n: Name): Process
    Par(p: Process, q: Process): Process
    Var(id: string): Name
    Quote(p: Process): Name
    new_(vlist: Array<vdecl>, body: Process): Process
}

function rhoBuilder(): RhoBuilder {
    const Nil = () => Object.freeze({
        _printOn: (out) => out.print("Nil"),
        quote: () => Quote(Nil())
    });
    const primitive = (v: boolean | number | string) => Object.freeze({
        _printOn: (out) => out.print(JSON.stringify(v)),
        quote: () => Quote(primitive(v))
    });
    const Quote = (p: Process) => Object.freeze({
        _printOn: (out) => {
            out.print("@{ ");
            p._printOn(out)
            out.print(" }")
        },
        deref: () => p
    });
    const Drop = (name: Name) => Object.freeze({
        _printOn: (out) => {
            out.print("*");
            name._printOn(out)
        },
        quote: () => name
    });
    const printList = (out: Printer, items: Array<Miranda>) => {
        let first = true;
        for (let item of items) {
            if (!first) {
                out.print(", ")
            }
            item._printOn(out)
            first = false
        }
    }
    const send = (dest: Name, procs: Array<Process>) => Object.freeze({
        _printOn: (out) => {
            dest._printOn(out)
            out.print(`!(`)  // TODO: !!
            printList(out, procs)
            out.print(")")
        },
        quote: () => Quote(send(dest, procs))
    });
    const binop = (op: BinOp, lhs: Process, rhs: Process) => Object.freeze({
        _printOn: (out) => {
            lhs._printOn(out)
            out.print(" " + op + " ")
            rhs._printOn(out)
        },
        quote: () => Quote(binop(op, lhs, rhs))
    })
    const receiving = (lhs: Array<Name>, rhs: Name, proc: Process) => Object.freeze({
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
    const contract = (name: Name, args: Array<Name>, body: Process) => Object.freeze({
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
    const Par = (p: Process, q: Process) => Object.freeze({
        _printOn: (out) => {
            p._printOn(out);
            out.print(" | ");
            q._printOn(out);
        },
        quote: () => Quote(Par(p, q))
    });
    const Var = (v: string) => Object.freeze({
        _printOn: (out) => out.print(v),
        deref: () => Drop(Var(v))
    })
    const new_ = (vlist: Array<string>, body: Process) => Object.freeze({
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
        Nil: Nil,
        primitive: primitive,
        send: send,
        binop: binop,
        receiving: receiving,
        contract: contract,
        Drop: Drop,
        Var: Var,
        Quote: Quote,
        Par: Par,
        new_: new_
    })
}

function makeCompiler(bld: RhoBuilder) {
    let tmp_ix = 0;
    const fresh = (n: Node) => `l${n.loc.start.line}c${n.loc.start.column}_${tmp_ix++}`

    const vn = (n: string) => bld.Var(n)
    const ignore = vn("_")  // TODO: this is a pattern, not a name, no?

    const par = (ps: Process[]) => ps.reduce((acc, next) => bld.Par(acc, next));

    function js2rho(js: Node, k: Name): Process {
        // console.log("DEBUG: node:", JSON.stringify(js));
        console.log("DEBUG: js.type", js.type);

        const recur = (js: Node) => {
            const kr = fresh(js)
            return { v: kr, p: js2rho(js, vn(kr)) }
        }
        const kont = (proc: Process) => bld.send(k, [proc]);
        const patname = (pat: Node) => {
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
                const vs: string[] = [].concat([target.v], args.map(a => a.v));
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
                console.log("@@not impl:", JSON.stringify(js, null, 2))
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
                console.log("@@not impl:", JSON.stringify(js, null, 2))
                return bld.primitive(js.type)
            case "Program":
                if (js.body.length == 1) {
                    return js2rho(js.body[0], k)
                }
            // TODO: thread statements
            default:
                console.log("@@not impl:", JSON.stringify(js, null, 2))
                return bld.primitive(js.type)
        }
    }
    return js2rho;
}

const tests = [
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
function unittest() {
    const bld = rhoBuilder();
    const compiler = makeCompiler(bld);
    const printer = Object.freeze({
        print: (txt: string) => process.stdout.write(txt)
    })

    for (const item of tests) {
        const prog: Program = parseScript(item.src, { loc: true });

        console.log("@@program:", prog);

        compiler(prog, bld.Var("top"))._printOn(printer);
    }
}

unittest();
