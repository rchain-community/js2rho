/// <reference path="node_modules/@types/node/index.d.ts" />
/// <reference path="node_modules/@types/esprima/index.d.ts" />
import { parseScript, Program } from "esprima";
import { Node } from "estree";

// console.log(process.argv); TODO: get filename from process.argv


const program = `
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
`;

const program0 = `
m.set(purse, decr);
`;

const program1 = `
const makeMint = () => obj.verb(1, 2)
`;

const prog: Program = parseScript(program1);

console.log("@@program:", prog);

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
interface RhoBuilder {
    Nil(): Process
    primitive(v: boolean | number | string): Process
    send(dest: Name, procs: Array<Process>): Process
    receiving(lhs: Array<Name>, rhs: Name, proc: Process): Process
    Drop(n: Name): Process
    Par(p: Process, q: Process): Process
    Var(id: string): Name
    Quote(p: Process): Name
}

function rhoBuilder(): RhoBuilder {
    const Nil = () => Object.freeze({
        _printOn: (out) => out.print("Nil"),
        quote: () => builder.Quote(this)
    });
    const primitive = (v: boolean | number | string) => Object.freeze({
        _printOn: (out) => out.print(JSON.stringify(v)),
        quote: () => builder.Quote(this)
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
    const send = (dest: Name, procs: Array<Process>) => Object.freeze({
        _printOn: (out) => {
            dest._printOn(out)
            out.print(`!(`)  // TODO: !!
            printList(out, procs)
            out.print(")")
        },
        quote: () => builder.Quote(this)
    });
    // TODO: multiple names etc.
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
        quote: () => builder.Quote(this)
    })
    const Par = (p: Process, q: Process) => Object.freeze({
        _printOn: (out) => {
            p._printOn(out);
            out.print(" | ");
            q._printOn(out);
        },
        quote: () => builder.Quote(this)
    });
    const Var = (v: string) => Object.freeze({
        _printOn: (out) => out.print(v),
        deref: () => builder.Drop(this)
    })
    return Object.freeze({
        Nil: Nil,
        primitive: primitive,
        send: send,
        receiving: receiving,
        Drop: Drop,
        Var: Var,
        Quote: Quote,
        Par: Par
    })
}

function makeCompiler(builder: RhoBuilder) {
    function js2rho(js: Node, k: Name): Process {
        // console.log("DEBUG: node:", JSON.stringify(js));
        console.log("DEBUG: js.type", js.type);
        switch(js.type) {
            case "Literal":
                const v = js.value
                if (typeof(v) == "object") {
                    throw(v)  // TODO: RegExp
                }
                return builder.primitive(v)
            case "Identifier":
                return builder.Drop(builder.Var(js.name))
            case "ExpressionStatement":
                return js2rho(js.expression, k);
            case "VariableDeclaration":
                const decls = js.declarations
                if (js.kind == "const" && decls.length == 1) {
                    const decl = decls[0]
                    if (decl.type == "VariableDeclarator") {
                        const id = decl.id
                        if (id.type == "Identifier") {
                            const lhs = builder.Var(id.name)
                            const init = decl.init
                            if (init.type == "ArrowFunctionExpression") {
                                const ignore = builder.Var("_")
                                const params = init.params.length == 0 ? [ignore] :(
                                    init.params.map(
                                        expr => js2rho(expr, k).quote()) ))
                                return builder.receiving(params, lhs, js2rho(init.body, k))
                            }
                            const rhs = js2rho(init, k)
                            return builder.send(lhs, [rhs])
                        }
                    }
                }
                console.log("@@not impl:", JSON.stringify(js, null, 2))
                return builder.primitive(js.type)
            case "CallExpression":
                const callee = js.callee
                const procs = js.arguments.map(arg => js2rho(arg, k))
                /* special case for obj.prop(...) */
                if (callee.type == "MemberExpression") {
                    const prop = callee.property
                    if (prop.type == "Identifier") {
                        const verbProcs = [].concat(
                            [builder.primitive(prop.name)], procs)
                        const c2 = js2rho(callee.object, k).quote()
                        return builder.send(c2, verbProcs)
                    }
                }
                const target = js2rho(js.callee, k).quote()
                return builder.send(target, procs)
            case "Program":
                let xx;
                for (let statement of js.body) {
                    // TODO: thread statements
                    xx = js2rho(statement, k);
                }
                return xx;
            default:
                console.log("@@not impl:", JSON.stringify(js, null, 2))
                return builder.primitive(js.type)
        }
    }
    return js2rho;
}

const builder = rhoBuilder();
const compiler = makeCompiler(builder);
const printer = Object.freeze({
    print: (txt: string) => process.stdout.write(txt)
})

compiler(prog, builder.Var("top"))._printOn(printer);