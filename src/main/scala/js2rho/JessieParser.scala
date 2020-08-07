/**
  * based on https://github.com/agoric-labs/jessica#grammar
  * formerly: https://github.com/Agoric/TinySES/blob/master/src/tinyses.js
  */
package js2rho

import scala.io.Source
import scala.util.matching.Regex
import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

// ISSUE: use JavaTokenParsers instead?
class JSONTokens extends RegexParsers {
  override def skipWhitespace = true
  override val whiteSpace: Regex = "(\\s*//[^\\n]*\\n\\s*)+|(\\s+)".r
  def NUMBER: Parser[Double] = raw"-?\d+(?:\.\d+)?(?:[eE]-?\d+)?".r ^^ { n =>
    n.toDouble
  }
  val UCODE_RE = raw"\\u[\da-fA-F]{4}"
  // Note no \' (escaped single quote) in accord with JSON.
  // TODO: val CHAR_RE = r"[^\"\\]|\\"|\\\\|\\/|\\b|\\f|\\n|\\r|\\t|" // TODO: + UCODE_RE
  // TODO: def STRING: Parser[String] = s"\"${CHAR_RE}*\"".r
  def STRING: Parser[String] = ("\"[^\"]*\"".r | "'[^']*'".r)
}

class JSONParser extends JSONTokens {
  def startExpr = phrase(assignExpr)

  // to be extended
  def primaryExpr = dataStructure

  def dataStructure = (dataLiteral
    | array
    | record
    //  | HOLE
  )

  // An expression without side-effects.
  // to be extended
  def pureExpr: Parser[Expression] = (dataLiteral
    | pureArray
    | pureRecord
    // | HOLE
  )

  def dataLiteral: Parser[DataLiteral] = positioned(
    "null" ^^ { _ =>
      Null
    }
      | "false" ^^ { _ => False }
      | "true" ^^ { _ => True }
      | NUMBER ^^ Number
      | STRING ^^ StrLit
  )

  def pureArray = "[" ~> repsep(pureExpr, ",") <~! "]" ^^ { args => ArrayExpr(args: _*) }
  def array = "[" ~> repsep(element, ",") <~! "]" ^^ { args => ArrayExpr(args: _*) }

  // to be extended
  def element: Parser[Expression] = assignExpr

  // The JavaScript and JSON grammars calls records "objects"
  def pureRecord: Parser[Expression] =
    "{" ~> repsep(purePropDef, ",") <~! "}" ^^ { ps => ObjectExpr(ps: _*) }

  def record: Parser[Expression] =
    "{" ~> repsep(propDef, ",") <~! "}" ^^ { ps => ObjectExpr(ps: _*) }

  // to be extended
  def purePropDef: Parser[Property] = (propName <~ ":") ~ pureExpr ^^ { case k ~ e => Prop(k, e) }
  // to be extended
  def propDef: Parser[Property] = (propName <~ ":") ~ assignExpr ^^ { case k ~ e => Prop(k, e) }

  // to be extended
  def propName: Parser[PropName] = STRING ^^ PropKey // TODO: if (js === '__proto__') return FAIL;

  // to be overridden
  def assignExpr: Parser[Expression] = primaryExpr
}

object QuasiParser {
  val all_pat = "`[^`$$]*`".r // TODO: ~"\${"
  val head_pat = "`[^$]*\\$\\{".r
  val mid_pat = "}[^$]*\\$\\{".r
  val tail_pat = "}[^$]*`".r
}

class JustinParser extends JSONParser {
  def binary[R](p: Expression ~ List[Expression => Expression]): Expression =
    p match {
      case left ~ rights =>
        rights.foldLeft(left) { (acc, item) => item(acc) }
    }

  // TODO: Error if whitespace includes newline
  def NO_NEWLINE: Parser[String] = "" // TODO

  def identName: Parser[String] = IDENT | RESERVED_WORD

  def IDENT: Parser[String] = not(RESERVED_WORD) ~> raw"[a-zA-Z_$$][\w$$]*".r

  // Omit "async", "arguments", and "eval" from IDENT in TinySES even
  // though ES2017 considers them in IDENT.
  def RESERVED_WORD: Parser[String] = (
    KEYWORD | RESERVED_KEYWORD | FUTURE_RESERVED_WORD
      | "null" | "false" | "true"
      | "async" | "arguments" | "eval"
  )

  def KEYWORD: Parser[String] = (
    "break"
      | "case" | "catch" | "const" | "continue"
      | "debugger" | "default"
      | "else" | raw"export\b".r
      | "finally" | "for" | "function"
      | "if" | "import"
      | "return"
      | "switch"
      | "throw" | "try" | "typeof"
      | "void"
      | "while"

      | "const" // ISSUE: added
  )

  // Unused by TinySES but enumerated here, in order to omit them
  // from the IDENT token.
  def RESERVED_KEYWORD: Parser[String] = (
    "class"
      | "delete" | "do"
      | "extends"
      | raw"in\b".r | "instanceof" // TODO: keyword boundaries
      | "new"
      | "super"
      | "this"
      | "var"
      | "with"
      | "yield"

      | "let" // ISSUE: added
  )

  def FUTURE_RESERVED_WORD: Parser[String] = (
    "await" | "enum"
      | "implements" | "package" | "protected"
      | "interface" | "private" | "public"
  )

  def QUASI_ALL: Parser[String] = QuasiParser.all_pat ^^ {
    case tok => tok.drop(1).dropRight(1)
  }
  def QUASI_HEAD: Parser[String] = QuasiParser.head_pat ^^ {
    case tok => tok.drop(1).dropRight(2)
  }
  def QUASI_MID: Parser[String] = QuasiParser.mid_pat ^^ {
    case tok => tok.drop(1).dropRight(2)
  }
  def QUASI_TAIL: Parser[String] = QuasiParser.tail_pat ^^ {
    case tok => tok.drop(1)
  }

  def qunpack(
    h: String,
    ms: Option[(Expression ~ List[String ~ Expression])],
      t: String
  ): Seq[Either[String, Expression]] = {
    Seq(Left(h)) ++ (ms match {
      case None => Seq()
      case Some(e ~ ses) => Seq(Right(e)) ++ ses.flatMap {
        case s ~ e => Seq(Left(s), Right(e))
      }
    })
  }

  def undefined = "undefined" ^^^ Undefined

  override def dataStructure = undefined | super.dataStructure;

  def useVar: Parser[Expression] = IDENT ^^ Use

  // only used in extensions of Justin
  def defVar: Parser[Pattern] = IDENT ^^ Def

  override def primaryExpr: Parser[Expression] = log(positioned(
    super.primaryExpr
      | log(quasiExpr)("quasiExpr")
      | "(" ~> expr <~! ")"
      | useVar
  ))("primaryExpr")

  override def pureExpr: Parser[Expression] = positioned(
    super.pureExpr
      | "(" ~> expr <~! ")"
      | useVar
  )

  override def element = super.element | "..." ~> expr ^^ SpreadExpr

  override def propDef = (super.propDef
    | useVar ^^ { case Use(id) => Prop(PropKey(id), Use(id)) }
    | ("..." ~> assignExpr) ^^ SpreadObj)
  override def purePropDef = (super.purePropDef
    | useVar ^^ { case Use(id) => Prop(PropKey(id), Use(id)) }
    | ("..." ~> assignExpr) ^^ SpreadObj)

  // No computed property name
  override def propName: Parser[PropName] = (
    super.propName
      | identName ^^ PropKey
      | NUMBER ^^ { n: Double => PropIx(n) }
  )

  def quasiExpr: Parser[Expression] = (
    QUASI_ALL ^^ { a => Quasi(Left(a)) }
      | QUASI_HEAD ~ ((expr ~ ((QUASI_MID ~ expr) *)) ?) ~ log(QUASI_TAIL)("QUASI_TAIL") ^^ {
        case h ~ ms ~ t => Quasi(qunpack(h, ms, t): _*)
      }
  )

  def expr = assignExpr
}

class JessieParser extends JustinParser {
  type Body = List[Either[Statement, Declaration]]
  type Bindings = List[(Pattern, Expression)]

  def startProgram: Parser[Program] = phrase(log(moduleBody)("moduleBody"))

  def later: Parser[String] = "~."  // TODO: followed by non-digit

  // For most identifiers that ES2017 treats as IDENT but recognizes
  // as pseudo-keywords in a context dependent manner, TinySES simply makes
  // keywords. However, this would be too painful for "get" and
  // "set", so instead we ???use our parser-generator's support syntactic
  // predicates. TODO: Is it really too painful? Try it.
  // identGet ::= IDENT ^^ { id => if (id == "get") { id } else { FAIL }}
  // identSet ::= IDENT ^^ { id => if (id == "set") { id } else { FAIL }}
  def identGet: Parser[String] = "get"
  def identSet: Parser[String] = "set"

  override def primaryExpr: Parser[Expression] = positioned(
    super.primaryExpr
      | functionExpr
  )

  def bindingPattern: Parser[Pattern] =
    positioned(
      "[" ~> repsep(param, ",") <~ "]" ^^ { MatchArray(_: _*) }
        | "{" ~> repsep(propParam, ",") <~ "}" ^^ { MatchObj(_: _*) }
    )

  def pattern: Parser[Pattern] =
    log(
      positioned(
        dataLiteral ^^ MatchData
          | bindingPattern
          | defVar
        // TODO: | HOLE                                  ^^ { _ => PatternHole }
      )
    )("pattern")

  def param: Parser[Pattern] =
    ("..." ~> pattern ^^ Rest
      | defVar ~ ("=" ~> expr) ^^ { case v ~ e => Optional(v, e) }
      | pattern)

  override def propDef: Parser[Property] = (
    methodDef
    | super.propDef
  )

  override def purePropDef: Parser[Property] = (
    methodDef
    | super.purePropDef
  )

  def propParam: Parser[PropParam] = (
    "..." ~> pattern ^^ RestObj
      | (propName <~ ":") ~ pattern ^^ { case k ~ p => MatchProp(k, p) }
      | (IDENT <~ "=") ~ expr ^^ { case id ~ e      => OptionalProp(id, id, e) }
      | IDENT ^^ { id => MatchProp(PropKey(id), Def(id)) }
  )

  // No "new", "super", or MetaProperty. Without "new" we don't need
  // separate MemberExpr and CallExpr productions.
  // Recognize b!foo(x) as distinct from calling b!foo post-parse.
  def postExpr: Parser[Expression] = primaryExpr ~ (postOp *) ^^ { binary }
  def postOp: Parser[Expression => Expression] = (
    "." ~> identName ^^ { id => e: Expression =>
      Get(e, id)
    }
      | "[" ~> indexExpr <~ "]" ^^ { ix => e: Expression => Index(e, ix) }
      | "(" ~> repsep(arg, ",") <~! ")" ^^ { args => e: Expression =>
        Call(e, args)
      }
      | quasiExpr ^^ { q => e: Expression => Tag(e, q) }
      | later ~> identName ^^ { id => e: Expression => GetLater(e, id) }
      | later ~> "[" ~> indexExpr <~ "]" ^^ { ix => e: Expression =>
        IndexLater(e, ix)
      }
      | later ~> "(" ~> repsep(arg, ",") <~! ")" ^^ { args => e: Expression =>
        CallLater(e, args)
      }
  )
  // Omit ("delete" fieldExpr) to avoid mutating properties
  def preExpr: Parser[Expression] = (
    preOp ~ preExpr ^^ { case op ~ e => op(e) }
      | postExpr
  )

  def op1(op: UnOp)(tok: String)(e: Expression) = Unary(op, e)
  // No prefix or postfix "++" or "--".
  // No "delete". No bitwise "~".
  def preOp: Parser[Expression => Expression] = (
    "void" ^^ { op1(Void) }
      | "typeof" ^^ { op1(TypeOf) }
      | "+" ^^ { op1(Positive) }
      | "-" ^^ { op1(Negative) }
      | "!" ^^ { op1(Not) }
  )

  // Restrict index access to number-names, including
  // floating point, NaN, Infinity, and -Infinity.
  def indexExpr: Parser[Expression] = "+" ~> preExpr ^^ { e =>
    Unary(Positive, e)
  }

  def arg: Parser[Expression] =
    (assignExpr
      | "..." ~> assignExpr ^^ SpreadExpr)

  // No bitwise operators, "instanceof", "in", "==", or "!=".  Unlike
  // ES8, none of the relational operators (including equality)
  // associate. To help readers, mixing relational operators always
  // requires explicit parens.
  // TODO: exponentiation "**" operator.
  def binStep(p: String ~ Expression)(lt: Expression): Expression = p match {
    case tok ~ rt => Binary(tok, lt, rt)
  }
  def multExpr: Parser[Expression] =
    preExpr ~ ((
      (("*" | "/" | "%") ~ preExpr) ^^ binStep
    ) *) ^^ { binary }

  def addExpr: Parser[Expression] =
    multExpr ~ (((("+" | "-") ~ multExpr) ^^ binStep) *) ^^ { binary }
  def relExpr: Parser[Expression] =
    addExpr ~ ((((relOp ~ addExpr) ^^ binStep) ?) ^^ { _.toList }) ^^ { binary }
  def relOp: Parser[String] = "<" | ">" | "<=" | ">=" | "===" | "!=="
  def andThenExpr: Parser[Expression] =
    relExpr ~ ((("&&" ~ relExpr) ^^ binStep) *) ^^ { binary }
  def orElseExpr: Parser[Expression] =
    andThenExpr ~ ((("||" ~ andThenExpr) ^^ binStep) *) ^^ { binary }
  // No trinary ("?:") expression
  // No comma expression, so assignment expression is expr.
  // TODO: Need to be able to write (1,array[i])(args), which
  // either requires that we readmit the comma expression, or
  // that we add a weird special case to the grammar.
  override def expr: Parser[Expression] =
    log(
      positioned(
        lValue ~ (assignOp ~ expr) ^^ {
          case lv ~ (op ~ rv) => Assign(op, lv, rv)
        }
          | arrow
          | orElseExpr
      )
    )("expr")

  override def assignExpr: Parser[Expression] = (
    arrowFunc
      | functionExpr
    // TODO: lValue postOp
    // TODO: lValue (EQUALS / assignOp) assignExpr
    // TODO: super.assignExpr
      | primaryExpr
  )

  // lValue is only useVar or elementExpr in TinySES.
  // Include only elementExpr from fieldExpr to avoid mutating
  // non-number-named properties.
  // Syntactically disallow ("delete" IDENT).
  // No pseudo-pattern lValues.
  // TODO: re-allow assignment to statically named fields,
  // since it is useful during initialization and prevented
  // thereafter by mandatory tamper-proofing.
  def lValue = elementExpr | useVar
  def elementExpr = (
    primaryExpr ~ ("[" ~> indexExpr <~! "]") ^^ { case pe ~ e => Index(pe, e) }
      | primaryExpr ~ (later ~> "[" ~> indexExpr <~! "]") ^^ {
        case pe ~ e => IndexLater(pe, e)
      }
  )
  def fieldExpr: Parser[Expression] = (
    primaryExpr ~ ("." ~> identName) ^^ { case pe ~ id => Get(pe, id) }
      | primaryExpr ~ (later ~> "." ~> identName) ^^ {
        case pe ~ id => GetLater(pe, id)
      }
      | elementExpr
  )
  // No bitwise operators
  def assignOp = "=" | "*=" | "|=" | "%=" | "+=" | "-="
  // The expr form must come after the block form, to make proper use
  // of PEG prioritized choice.
  def arrow: Parser[Expression] = (
    params ~ (NO_NEWLINE ~> "=>" ~> block) ^^ { case ps ~ b => Arrow(ps, b) }
      | params ~ (NO_NEWLINE ~> "=>" ~> expr) ^^ {
        case ps ~ e => Lambda(ps, e)
      }
  )
  def params: Parser[List[Pattern]] = (
    IDENT ^^ { id =>
      List(Def(id))
    }
      | "(" ~> repsep(param, ",") <~! ")"
  )

  // No "var", empty statement, "with", "do/while", or "for/in". None
  // of the insane variations of "for". Only blocks are accepted for
  // flow-of-control statements.
  // The expr production must go last, so PEG's prioritized choice will
  // interpret {} as a block rather than an expression.
  def statement: Parser[Statement] =
    log(
      positioned(
        block
        /** TODO
      | "if" ~> ("(" ~> expr <~ ")") ~ block ~ ("else" ~> block)? ^^ { case c ~ t ~ e => If(c, t, e) }
      | "for" ~> "(" ~> declaration ~> expr? ~ (";" ~> expr?) ~ ")" ~ block    ^^ { case d ~ c ~ i ~ b => For(d, c, i, b) }
      | "for" ~> "(" ~> declOp ~ binding ~ ("of" ~> expr <~ ")") ~ block       ^^ { case d ~ e ~ b => ForOf(d, e, b) }
      | "while" ~> ("(" ~> expr <~ ")") ~ block               ^^ { case c ~ b => While(c, b) }
      | "switch" ~> ("(" ~> expr <~ ")") ~ ("{" ~> (branch*) <~ "}") ^^ { case e ~ bs => Switch(e, bs) }
      | (IDENT <~ ":") ~ statement                           ^^ { case label ~ stat => Label(label, stat) }
      | "try" ~> block ~ catcher? ~ finalizer?                 ^^ { case b ~ c ~ f => Try(b, c, f) }
            */
          | terminator
        /** TODO
      | "debugger" <~! ";"                                    ^^ { Debugger }
            */
          | expr <~! ";" ^^ { e => ExprStmt(e) }
      )
    )("statement")

  // Each case branch must end in a terminating statement.
  def terminator: Parser[Statement] = (
    "return" ~> ((NO_NEWLINE ~> expr) ?) <~! ";" ^^ { Return(_) }
      | "break" ~> ((NO_NEWLINE ~> IDENT) ?) <~! ";" ^^ { Break(_) }
      | "continue" ~> ((NO_NEWLINE ~> IDENT) ?) <~! ";" ^^ { Continue(_) }
      | "throw" ~> expr <~! ";" ^^ { Throw(_) }
  )

  // No "class" declaration.
  // No generator, async, or async iterator function.
  def declaration: Parser[Declaration] =
    log(
      declOp ~ repsep(binding, ",") <~! ";" ^^ { case op ~ decls => op(decls) }
        | functionDecl
    )("declaration")
  def declOp: Parser[Bindings => Declaration] = (
    "const" ^^^ { Const }
      | "let" ^^^ { Let }
  )
  // Initializer is mandatory
  def binding = pattern ~ ("=" ~> expr) ^^ { case p ~ e => (p, e) }

  /** TODO
  def catcher = "catch" ~> ("(" ~> pattern <~ ")") ~ block   ^^ { case p ~ b => Catch(p, b) }
  def finalizer = "finally" ~> block                         ^^ { Finally(_) }
  def branch = caseLabel+ ~ ("{" ~> body ~ terminator ~ "}") ^^ { case cs ~ b ~ t =>
    Branch(cs, b ++ List(t)) }
  def caseLabel = (
  "case" ~> expr <~ ":"                                  ^^ { Case(_) }
    | "default" ~ ":"                                    ^^ { Default(_) }
  )
    **/
  def block: Parser[Block] = log("{" ~> body <~ "}")("block") ^^ { Block(_) }
  def body: Parser[Body] =
    rep(statement ^^ { Left(_) } | declaration ^^ { Right(_) })
  def functionExpr: Parser[Expression] =
    "function" ~> (defVar ?) ~ ("(" ~> repsep(param, ",") <~! ")") ~ block ^^ {
      case n ~ p ~ b => FunctionExpr(n, p, b)
    }

  def functionDecl: Parser[Declaration] =
    "function" ~> defVar ~ ("(" ~> repsep(param, ",") <~! ")") ~ block ^^ {
      case n ~ p ~ b => FunctionDecl(n, p, b)
    }

  def arrowFunc: Parser[Expression] = (
    (arrowParams <~ /* TODO: NO_NEWLINE */ "<-") ~ block ^^ {
      case ps ~ b => Arrow(ps, b)
    }
      | (arrowParams /* TODO: NO_NEWLINE */ <~ "<-") ~ assignExpr ^^ {
        case ps ~ e => Lambda(ps, e)
      }
  )
  def arrowParams: Parser[List[Pattern]] = (
    IDENT ^^ { case i => List(Def(i)) }
      | ("(" ~> repsep(param, ",") <~ ")")
  )
  def methodDef: Parser[Property] = (
    propName ~ ("(" ~> repsep(param, ",") <~! ")") ~ block ^^ {
      case n ~ p ~ b => MethodDef(n, p, b)
    }
      | identGet ~> (propName <~ "(" <~! ")") ~ block ^^ {
        case n ~ b => Getter(n, List(), b)
      }
      | identSet ~> propName ~ ("(" ~> param <~! ")") ~ block ^^ {
        case n ~ p ~ b => Setter(n, List(p), b)
      }
  )

  def moduleBody: Parser[Program] =
    rep(statement ^^ { s =>
      Left(s)
    } | moduleItem ^^ { m => Right(m) }) ^^ { case items => Program(items) }
  def moduleItem: Parser[ModuleDeclaration] =
    // SEMI -> SKIP
    log(importDecl | exportDecl | moduleDeclaration)("moduleItem")

  def moduleDeclaration: Parser[ModuleDeclaration] =
    (
      "const" ~> repsep(moduleBinding, ",") <~ ";"
    ) ^^ { case decls => Const(decls) }

  def moduleBinding: Parser[(Pattern, Expression)] = (
    (bindingPattern <~ "=") ~ hardenedExpr ^^ { case p ~ e => (p, e) }
      | (defVar <~ "=") ~ hardenedExpr ^^ { case p ~ e     => (p, e) }
      | defVar ^^ { case Def(id)                           => (Def(id), Use(id)) }
  )
  def hardenedExpr: Parser[Expression] =
    log(
      dataLiteral | undefined
        | ("harden" ~> "(" ~> (pureExpr | useImport) <~ ")")
        | useVar
    )("hardenedExpr")
  def useImport: Parser[Expression] = IDENT ^^ { case i => Use(i) }
  override def pureExpr =
    log(
      arrowFunc
        | super.pureExpr
    )("pureExpr")

  def importDecl: Parser[ImportDeclaration] =
    ("import" ~> importClause) ~ ("from" ~> (STRING ^^ StrLit)) <~ ";" ^^ {
      case specifiers ~ source => ImportDeclaration(specifiers, source)
    }
  def importClause: Parser[List[ModuleSpecifier]] = (
    // STAR
    namedImports
      | defImport ^^ { case spec => List(spec) }
    // TODO ...
  )
  def namedImports: Parser[List[ModuleSpecifier]] =
    "{" ~> repsep(importSpecifier, ",") <~ opt(",") <~ "}"
  def importSpecifier: Parser[ModuleSpecifier] =
    (IDENT ~ ("as" ~> defImport) ^^ {
      case i ~ ImportSpecifier(l, _) => ImportSpecifier(l, Identifier(i))
    }
      | defImport)
  def defImport: Parser[ImportSpecifier] = IDENT ^^ {
    case i => ImportSpecifier(Identifier(i), Identifier(i))
  }

  def exportDecl: Parser[ModuleDeclaration] = (
    ("export" ~> "default" ~> exportableExpr <~ ";") ^^ {
      case e => ExportDefaultDeclaration(e)
    }
      | "export" ~> moduleDeclaration
  )
  def exportableExpr = hardenedExpr
}
