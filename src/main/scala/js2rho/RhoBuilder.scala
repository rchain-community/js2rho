package js2rho

import java.io.PrintStream

import spray.json._
import DefaultJsonProtocol._

/**
  * https://github.com/rchain/rchain/blob/dev/rholang/src/main/bnfc/rholang_mercury.cf
  */
trait Miranda {
  def _printOn(p: Printer): Unit
}

trait Printer {
  def print(s: String): Unit
  def begin(s: String): Unit
  def newline(): Unit
  def end(s: String): Unit
}

object Printer {
  def fromPrintStream(out: PrintStream): Printer = {
    var indent = 0;
    return new Printer {
      def print(txt: String) { out.print(txt) }
      def begin(txt: String) { out.print(txt); indent += 1; this.newline() }
      def newline() { out.print("\n" + "  ".repeat(indent)) }
      def end(txt: String) {
        this.newline(); out.print(txt); indent -= 1; this.newline()
      }
    }
  }

  def inMemory(buf: StringBuilder): Printer = {
    var indent = 0;
    return new Printer {
      def print(txt: String) { buf.append(txt) }
      def begin(txt: String) { buf.append(txt); indent += 1; this.newline() }
      def newline() { buf.append("\n" + "  ".repeat(indent)) }
      def end(txt: String) {
        this.newline(); buf.append(txt); indent -= 1; this.newline()
      }
    }
  }
}

trait Process extends Miranda {
  def quote(): Name
}

trait Name extends Miranda {
  def deref(): Process
}

trait RhoBuilder {
  def Nil(): Process
  def primitive(value: Boolean): Process
  def primitive(value: Int): Process
  def primitive(value: String): Process

  def Var(id: String): Name
  def Quote(p: Process): Name
  def Drop(n: Name): Process

  def Par(p: Process, q: Process): Process
  def receiving(rx: Seq[(Seq[Name], Name)], body: Process): Process
  def new_(vars: Seq[(String, Option[String])], body: Process): Process
  def send(dest: Name, procs: Seq[Process]): Process

  /*
 * @property {(procs: Process[]) => Process} listExpr
 * @property {(procs: Process[]) => Process} tupleExpr
 * @property {(entries: Array<{ key: string, value: Process }>) => Process} mapExpr
 * @property {(specimen: Process, cases: { lhs: Process, rhs: Process }) => Process } match
 * @property {(op: BinOp, lhs: Process, rhs: Process) => Process} binop
 * @property {(op: UnOp, arg: Process) => Process} unary
 * @property {(name: Name, args: Array<Name>, body: Process) => Process} contract
 * @property {(n: Name) => Process} Drop
 * @property {(p: Process, q: Process) => Process} Par
 * @property {(p: Process) => Name} Quote
 *
 * @typedef {{ lhs: Name[], rhs: Name}[]} Receipt

 */
}

class RhoPrinter extends RhoBuilder {
  def aNil(): Process = new Process {
    def _printOn(out: Printer) = out.print("Nil")
    def quote() = Quote(aNil())
  }
  val theNil = aNil()
  def Nil() = theNil

  def primitive(v: String) = new Process {
    def _printOn(out: Printer) = out.print(v.toJson.toString())
    def quote() = Quote(primitive(v))
  }
  def primitive(v: Int) = new Process {
    def _printOn(out: Printer) = out.print(v.toJson.toString())
    def quote() = Quote(primitive(v))
  }
  def primitive(v: Boolean) = new Process {
    def _printOn(out: Printer) = out.print(v.toJson.toString())
    def quote() = Quote(primitive(v))
  }

  def Quote(p: Process) = new Name {
    def _printOn(out: Printer) = {
      out.print("@{ ")
      p._printOn(out)
      out.print(" }")
    }
    def deref() = p
  }
  def Drop(name: Name) = new Process {
    def _printOn(out: Printer) = {
      out.print("*")
      name._printOn(out)
    }
    def quote() = name
  }
  def Var(v: String) = new Name {
    def _printOn(out: Printer) = out.print(v)
    def deref() = Drop(Var(v))
  }

  def Par(p: Process, q: Process) =
    if (p == theNil) { q }
    else if (q == theNil) { p }
    else
      new Process {
        def _printOn(out: Printer) = {
          p._printOn(out)
          out.newline()
          out.print("|")
          out.newline()
          q._printOn(out)
        }
        def quote() = Quote(Par(p, q))
      }

  def receiving(rx: Seq[(Seq[Name], Name)], proc: Process) = new Process {
    def _printOn(out: Printer) = {
      out.print("for(")
      var first = true;
      for ((lhs, rhs) <- rx) {
        if (!first) {
          out.print("; ")
        }
        printList(out, lhs)
        out.print(" <- ")
        rhs._printOn(out)
        first = false;
      }
      out.begin(") {")
      proc._printOn(out)
      out.end("}")
    }
    def quote() = Quote(receiving(rx, proc))
  }
  def printList(out: Printer, items: Iterable[Miranda]) = {
    var first = true;
    for (item <- items) {
      if (!first) {
        out.print(", ")
      }
      item._printOn(out)
      first = false
    }
  }

  /** @type {(vlist: vdecl[], body: Process) => Process} */
  def new_(vars: Seq[(String, Option[String])], body: Process): Process = new Process {
    def _printOn(out: Printer) = {
      out.newline();
      out.print("new ")
      var first = true;
      for ((local, uri) <- vars) {
        uri match {
          case Some(i) => {
            if (!first) {
              out.print(",");
              out.newline();
            }
            out.print(s"$local(`$i`)");
          }
          case None => {
              if (!first) {
                out.print(", ");
              }
              out.print(local);

            }

            first = false;
        }
      }
      out.newline();
      out.begin("in {")
      body._printOn(out)
      out.end("}");
    }
    def quote() = Quote(new_(vars, body))
  }

  def send(dest: Name, procs: Seq[Process]): Process = new Process {
    def _printOn(out: Printer) = {
      dest._printOn(out)
      out.print("!(") // TODO: !!
      printList(out, procs)
      out.print(")")
    }
    def quote() = Quote(send(dest, procs))
  }

  /*
    const listExpr = (procs) => harden({
        _printOn(out) {
            out.print('[');
            printList(out, procs);
            out.print(']');
        },
        quote: () => Quote(listExpr(procs))
    });
    const tupleExpr = (procs) => harden({
        _printOn(out) {
            out.print('(');
            printList(out, procs);
            out.print(')');
        },
        quote: () => Quote(tupleExpr(procs))
    });
    const mapExpr = (entries) => harden({
        _printOn(out) {
            let first = true;
            out.print('{');
            for (const { key, value } of entries) {
                if (!first) {
                    out.print(', ')
                }
                out.print(JSON.stringify(key));
                out.print(': ');
                value._printOn(out);
                first = false;
            }
            out.print('}');
        },
        quote: () => Quote(mapExpr(entries))
    });
    const match = (specimen, cases) => harden({
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
        }

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

    const binop = (op, lhs, rhs) => harden({
        _printOn: (out) => {
            lhs._printOn(out)
            out.print(" " + op + " ")
            rhs._printOn(out)
        },
        quote: () => Quote(binop(op, lhs, rhs))
    })
    const unary = (op, arg) => harden({
        _printOn: (out) => {
            out.print(op);
            out.print('{');
            arg._printOn(out);
            out.print('}')
        },
        quote: () => Quote(unary(op, arg))
    })
    const contract = (name, args, body) => harden({
        _printOn: (out) => {
            out.print("contract ")
            name._printOn(out)
            out.print(`(`)
            printList(out, args)
            out.begin(`) = {`);
            body._printOn(out);
            out.end("}")
        },
        quote: () => Quote(contract(name, args, body))
    })

    return harden({
        Nil,
        primitive,
        listExpr,
        tupleExpr,
        mapExpr,
        match,
        send,
        binop,
        unary,
        receiving,
        contract,
        Drop,
        Var,
        Quote,
        Par,
        new_,
    });
}
 */
}
