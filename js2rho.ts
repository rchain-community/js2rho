/// <reference path="node_modules/@types/node/index.d.ts" />
/// <reference path="node_modules/@types/esprima/index.d.ts" />
import { parseScript, Program } from "esprima";
import { Node } from "estree";

console.log(process.argv);


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

const prog: Program = parseScript(program0);

console.log("program:", prog);

interface Miranda {
    _printOn(out: Printer)
}
interface Printer extends Miranda {
    print(s: string)
}
interface Process extends Miranda {
}
interface RhoBuilder {
    Nil(): Miranda
    Value(v: boolean | number | string): Miranda
    Lift(dest: string, proc: Process): Miranda
}

function rhoBuilder(): RhoBuilder {
    return Object.freeze({
        "Nil": () => Object.freeze({
            "_printOn": (out) => out.print("Nil")
        }),
        "Value": (v: boolean | number | string) => Object.freeze({
            "_printOn": (out) => out.print(v) // TODO: string quotes
        }),
        "Lift": (dest: string, proc: Process) => Object.freeze({
            "_printOn": (out) => {
                out.print(`$dest!(`)
                proc._printOn(out)
                out.print(")")
            }
        }),
    })
}

function makeCompiler(builder: RhoBuilder) {
    function js2rho(js: Node, k: string): Process {
        console.log("DEBUG: js.type", js.type);
        switch(js.type) {
            case "Program":
                let xx;
                for (let statement of js.body) {
                    // TODO: thread statements
                    xx = js2rho(statement, k);
                }
                return xx;
            case "ExpressionStatement":
                return js2rho(js.expression, k);
            case "CallExpression":
                return builder.Lift(String(js.loc),
                                    js2rho(js.arguments[0], k));
            default:
                console.log("@@not impl:", js.type)
        }
    }
    return js2rho;
}

const compiler = makeCompiler(rhoBuilder());
console.log("result:", compiler(prog, "top"));
