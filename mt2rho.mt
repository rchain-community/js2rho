exports (main)

def varFirst :DeepFrozen := [for ch in ('a'..'z' | 'A'..'Z' | '_'..'_' |'\''..'\'' ) ch].asSet()
def varRest :DeepFrozen := [for ch in ('a'..'z' | 'A'..'Z' | '0'..'9' | '_'..'_' |'\''..'\'' ) ch].asSet()


object RhoVar as DeepFrozen:
    to coerce(s, ej):
        if (s.size() == 0):
            throw.eject(ej, "empty string is not a RhoVar")
        if (!varFirst.contains(s[0])):
            throw.eject(ej, `RhoVar ${s} cannot start with ${s[0]}`)
        for ch in (s.slice(1)):
            if (! varRest.contains(ch)):
                throw.eject(ej, `RhoVar ${s} cannot contain $ch`)
        return s


# TODO: real unit test
def testRhoVar() as DeepFrozen:
    for specimen in (["abc", "123", "a*b"]):
        escape notVar:
            def _ :RhoVar exit notVar := specimen 
            traceln("name ok:", specimen)
        catch oops:
            traceln(oops)


interface Proc :DeepFrozen {}

def printList(out, items :List, "sep" => sep := ", ") as DeepFrozen:
    for ix => item in (items):
        if (ix > 0):
            out.print(sep)
        out.print(item)

# cf. https://github.com/rchain/rchain/blob/dev/rholang/src/main/bnfc/rholang_mercury.cf
object rhoBuilder as DeepFrozen:
    to Ground(v :Any[Bool, Int, Double, Str]):
        return object Ground as Proc:
            to printOn(out):
                switch (v):
                    match s :Str:
                        out.print(M.toQuote(s))  # ISSUE: string escapes?
                    match _:
                        out.print(v)

    to Nil():
        return object Nil as Proc:
            to printOn(out):
                out.print("Nil")

    to NameDecl(v :RhoVar):
        return object NameDecl:
            to printOn(out):
                out.print(v)

    to PNew(nameDecls :List[Str], proc: Proc):
        return object PNew:
            to printOn(out):
                out.print("new ")
                printList(out, nameDecls)
                out.print(" in ")
                out.print(proc)


# TODO: Expr guard
def translate(expr, kont, builder) as DeepFrozen:
    # if (expr == null):
    #     return builder.Nil()
    switch (expr.getNodeName()):
        match =="SeqExpr":
            def exs := expr.getExprs()
            return switch (exs.size()):
                match 0:
                    builder.Nil()
                match 1:
                    translate(exs[0], kont, builder)
                match _:
                    for ix => ex in (expr.getExprs().reverse()):
                        #@@@@@@@@
                        if (ix == 0):
                            translate(ex, kont, builder)
                        else:
                            out := builder.New(`seq$ix`, )
                            translate(ex)
                    
        match _:
            throw("not implemented: ", expr.getNodeName(), expr)

def obj1 :DeepFrozen := m`object origin {
    method getX() { 0 }
    method getY() { 0 }
}
# Now invoke the methods
origin.getY()
`

def main(_argv) as DeepFrozen:
    testRhoVar()
    traceln("monte:", obj1)
    def rho := translate(obj1.expand(), rhoBuilder)
    traceln("rholang", M.toString(rho))
