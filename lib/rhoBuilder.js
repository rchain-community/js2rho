// @ts-check

import harden from "@agoric/harden";

/**
 * @typedef {Object} Writer
 * @property {(s: string) => void} write
 *
 * @typedef {Object} Miranda
 * @property {(out: Printer) => void} _printOn
 *
 * @typedef {Object} Printer
 * @property {(s: string) => void} print
 * @property {(s: string) => void} begin
 * @property {() => void} newline
 * @property {(s: string) => void} end
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
 * @typedef {'bundle+'} UnOp
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
 * @property {(procs: Process[]) => Process} tupleExpr
 * @property {(entries: Array<{ key: string, value: Process }>) => Process} mapExpr
 * @property {(specimen: Process, cases: { lhs: Process, rhs: Process }[] ) => Process } match
 * @property {(dest: Name, procs: Array<Process>) => Process} send
 * @property {(op: BinOp, lhs: Process, rhs: Process) => Process} binop
 * @property {(op: UnOp, arg: Process) => Process} unary
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
  const aNil = () =>
    harden({
      _printOn: (out) => out.print("Nil"),
      quote: () => Quote(aNil()),
    });
  const theNil = aNil();
  const Nil = () => theNil;
  /** @type { (p: Process) => Name } */
  const Quote = (p) =>
    harden({
      _printOn: (out) => {
        out.print("@{ ");
        p._printOn(out);
        out.print(" }");
      },
      deref: () => p,
    });
  const printList = (out, items) => {
    let first = true;
    for (const item of items) {
      if (!first) {
        out.print(", ");
      }
      item._printOn(out);
      first = false;
    }
  };
  const fmtvdecl = (vd) =>
    typeof vd === "string" ? vd : `${vd[0]}(\`${vd[1]}\`)`;

  /** @type { RhoBuilder } */
  const self = {
    Nil,
    primitive: (v) =>
      harden({
        _printOn: (out) => out.print(JSON.stringify(v)),
        quote: () => Quote(self.primitive(v)),
      }),
    listExpr: (procs) =>
      harden({
        _printOn(out) {
          out.print("[");
          printList(out, procs);
          out.print("]");
        },
        quote: () => Quote(self.listExpr(procs)),
      }),
    tupleExpr: (procs) =>
      harden({
        _printOn(out) {
          out.print("(");
          printList(out, procs);
          out.print(")");
        },
        quote: () => Quote(self.tupleExpr(procs)),
      }),
    mapExpr: (entries) =>
      harden({
        _printOn(out) {
          let first = true;
          out.print("{");
          for (const { key, value } of entries) {
            if (!first) {
              out.print(", ");
            }
            out.print(JSON.stringify(key));
            out.print(": ");
            value._printOn(out);
            first = false;
          }
          out.print("}");
        },
        quote: () => Quote(self.mapExpr(entries)),
      }),
    match: (specimen, cases) =>
      harden({
        _printOn(out) {
          out.newline();
          out.print("match (");
          specimen._printOn(out);
          out.begin(") {");
          for (const { lhs, rhs } of cases) {
            lhs._printOn(out);
            out.begin(" => {");
            rhs._printOn(out);
            out.end("}");
          }
          out.end("}");
        },
      }),
    send: (dest, procs) =>
      harden({
        _printOn: (out) => {
          dest._printOn(out);
          out.print(`!(`); // TODO: !!
          printList(out, procs);
          out.print(")");
        },
        quote: () => Quote(self.send(dest, procs)),
      }),
    binop: (op, lhs, rhs) =>
      harden({
        _printOn: (out) => {
          lhs._printOn(out);
          out.print(" " + op + " ");
          rhs._printOn(out);
        },
        quote: () => Quote(self.binop(op, lhs, rhs)),
      }),
    unary: (op, arg) =>
      harden({
        _printOn: (out) => {
          out.print(op);
          out.print("{");
          arg._printOn(out);
          out.print("}");
        },
        quote: () => Quote(self.unary(op, arg)),
      }),
    receiving: (rx, proc) =>
      harden({
        _printOn: (out) => {
          out.print("for(");
          let first = true;
          for (const { lhs, rhs } of rx) {
            if (!first) {
              out.print("; ");
            }
            printList(out, lhs);
            out.print(` <- `);
            rhs._printOn(out);
            first = false;
          }
          out.begin(`) {`);
          proc._printOn(out);
          out.end("}");
        },
        quote: () => Quote(self.receiving(rx, proc)),
      }),
    contract: (name, args, body) =>
      harden({
        _printOn: (out) => {
          out.print("contract ");
          name._printOn(out);
          out.print(`(`);
          printList(out, args);
          out.begin(`) = {`);
          body._printOn(out);
          out.end("}");
        },
        quote: () => Quote(self.contract(name, args, body)),
      }),
    Drop: (name) =>
      harden({
        _printOn: (out) => {
          out.print("*");
          name._printOn(out);
        },
        quote: () => name,
      }),
    Var: (v) =>
      harden({
        _printOn: (out) => out.print(v),
        deref: () => self.Drop(self.Var(v)),
      }),
    Quote,
    Par: (p, q) =>
      p === theNil
        ? q
        : q === theNil
        ? p
        : harden({
            _printOn: (out) => {
              p._printOn(out);
              out.newline();
              out.print("|");
              out.newline();
              q._printOn(out);
            },
            quote: () => Quote(self.Par(p, q)),
          }),
    new_: (vlist, body) =>
      harden({
        _printOn: (out) => {
          out.newline();
          out.print("new ");
          let first = true;
          for (const item of vlist) {
            if (!first) {
              if (typeof item === "string") {
                out.print(", ");
              } else {
                out.print(",");
                out.newline();
              }
            }
            out.print(fmtvdecl(item));
            first = false;
          }
          out.newline();
          out.begin("in {");
          body._printOn(out);
          out.end("}");
        },
        quote: () => Quote(self.new_(vlist, body)),
      }),
  };

  return harden(self);
}
