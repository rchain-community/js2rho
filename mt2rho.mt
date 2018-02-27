# mt2rho -- Monte to Rholang compiler
# https://github.com/rchain/bounties/issues/427
#
# tabs:
# http://monte.readthedocs.io/en/latest/
# https://github.com/monte-language/typhon/blob/master/mast/fun/mli.mt
# https://developer.rchain.coop/tutorial
# https://github.com/rchain/rchain/blob/dev/rholang/examples/linking/packages/IArrayApi.rho
# https://github.com/rchain/rchain/blob/dev/rholang/src/main/bnfc/rholang_mercury.cf

exports (main)

def Expr :DeepFrozen := astBuilder.getExprGuard()

def letter :DeepFrozen := 'a'..'z' | 'A'..'Z'
def digit :DeepFrozen := '0'..'9'
def cr := fn c :Char { c..c }
def varFirst :DeepFrozen := [for ch in (letter | cr('_') | cr('\'') ) ch].asSet()
def varRest :DeepFrozen := [for ch in (letter | digit | cr('_') | cr('\'') ) ch].asSet()


object RhoVar as DeepFrozen:
    "token Var ((letter | '_')(letter | digit | '_' | '\'')*) ;"
    to coerce(x, ej):
        def s:Str exit ej := x
        if (s.size() == 0):
            throw.eject(ej, "empty string is not a RhoVar")
        def [first, rest] := [s[0], s.slice(1)]
        if (!varFirst.contains(first)):
            throw.eject(ej, `RhoVar ${s} cannot start with ${s[0]}`)
        for ch in (rest):
            if (! varRest.contains(ch)):
                throw.eject(ej, `RhoVar ${s} cannot contain $ch`)
        return s


def printList(out, items :List, "sep" => sep := ", ") as DeepFrozen:
    for ix => item in (items):
        if (ix > 0):
            out.print(sep)
        out.print(item)


# marker interfaces
interface Proc :DeepFrozen {}
interface RhoBuilder :DeepFrozen {}

object rhoBuilder as DeepFrozen implements RhoBuilder:
    to Nil():
        return object Nil as Proc:
            to _printOn(out):
                out.print("Nil")

    to Value(v :Any[Bool, Int, Double, Str]):
        if (v =~ s:Str):
            return object Value as Proc:
                to _printOn(out):
                    out.print(M.toQuote(s))  # ISSUE: same string escapes?
        else:
            return object Value as Proc:
                to _printOn(out):
                    out.print(v)

    to Lift(chan: RhoVar, proc: Proc):
        return object Lift as Proc:
            to _printOn(out):
                out.print(`$chan!(`)
                proc._printOn(out)
                out.print(")")

    to Input(pat: RhoVar, chan: RhoVar, proc: Proc):
        return object Input as Proc:
            to _printOn(out):
                out.print(`for($pat <- $chan) {$\n`)
                proc._printOn(out)
                out.print("\n}")

    to New(nameDecls :List[RhoVar], proc: Proc):
        return object New as Proc:
            to _printOn(out):
                out.print("new ")
                printList(out, nameDecls)
                out.print(" in {\n")
                proc._printOn(out)
                out.print("\n}")

    to Par(lhs: Proc, rhs: Proc):
        return object Par as Proc:
            to _printOn(out):
                lhs._printOn(out)
                out.print(" | ")
                rhs._printOn(out)


def butLast(specimen: List, ej) as DeepFrozen:
    def qty := specimen.size()
    if (qty == 0):
        throw.eject(ej, "butLast on empty list")
    return [specimen.slice(0, qty - 1), specimen.last()]


def translate(expr: Expr, dest: RhoVar, builder: RhoBuilder) as DeepFrozen:

    def doExprs(exs: List, dest):
        return switch(exs):
            match []:
                builder.Nil()
            match [ex]:
                translate(ex, dest, builder)
            match via (butLast) [init, tail]:
                def pat :RhoVar := `step${init.size()}`
                def chan :RhoVar := `${pat}Ch`
                def rhoInit := doExprs(init, chan)
                def rhoTail := translate(tail, dest, builder)
                builder.New([chan],
                            builder.Par(
                                rhoInit,
                                builder.Input(pat, chan, rhoTail)))
        
    return switch (expr.getNodeName()):
        match =="LiteralExpr":
            builder.Lift(dest, builder.Value(expr.getValue()))
        match =="SeqExpr":
            doExprs(expr.getExprs(), dest)
                
        match _:
            throw("not implemented: ", expr.getNodeName(), expr)


def monte0 :DeepFrozen := m`"feed"; "water"; "grow"`
def monte1 :DeepFrozen := m`object origin {
    method getX() { 0 }
    method getY() { 0 }
}
# Now invoke the methods
origin.getY()
`

# TODO: real unit test
def testRhoVar() as DeepFrozen:
    for specimen in (["abc", "123", "a*b"]):
        escape notVar:
            def _ :RhoVar exit notVar := specimen 
            traceln("name ok:", specimen)
        catch oops:
            traceln(oops)


def main(_argv) as DeepFrozen:
    testRhoVar()
    traceln("monte:", monte0)
    def rho := rhoBuilder.New(["return"],
                              translate(monte0.expand(), "return", rhoBuilder))
    traceln("rholang", rho)
