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
  // TODO: finish multi-line commentr regex
  override val whiteSpace: Regex = "(\\s*//[^\\n]*\\n\\s*)+|\\s*/\\*[^*]*\\*/|(\\s+)".r
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
    // TODO:  | HOLE
  )

  def dataLiteral: Parser[DataLiteral] = positioned(
    "null" ^^^ Null
      | "false" ^^^ False
      | "true" ^^^ True
      | NUMBER ^^ Number
      | STRING ^^ StrLit
  )

  def array = "[" ~> repsep(element, ",") <~! "]" ^^ { args => ArrayExpr(args: _*) }

  // to be extended
  def element: Parser[Expression] = assignExpr

  // The JavaScript and JSON grammars calls records "objects"
  def record: Parser[Expression] =
    "{" ~> repsep(propDef, ",") <~! "}" ^^ { ps => ObjectExpr(ps: _*) }

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

  override def startExpr = assignExpr

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

  // TODO: strip ``s?
  def qunpack(h: String, e: Expression, ms: List[String ~ Expression], t: String):
      Seq[Either[String, Expression]] = Seq(Left(h), Right(e)) ++ ms.flatMap {
    case s ~ e => Seq(Left(s), Right(e))
  } ++ Seq(Left(t))

  def undefined = "undefined" ^^^ Undefined

  override def dataStructure = undefined | super.dataStructure;

  // TODO: trailing commas in record, array

  def useVar: Parser[Expression] = IDENT ^^ Use

  // only used in extensions of Justin
  def defVar: Parser[Pattern] = IDENT ^^ Def

  override def primaryExpr: Parser[Expression] = positioned(
    super.primaryExpr
      | quasiExpr
      | "(" ~> expr <~! ")"
      | useVar
  )

  override def element = super.element | "..." ~> assignExpr ^^ SpreadExpr

  override def propDef = (super.propDef
    | useVar ^^ { case Use(id) => Prop(PropKey(id), Use(id)) }
    | ("..." ~> assignExpr) ^^ SpreadObj)

  // No computed property name
  override def propName: Parser[PropName] = (
    super.propName
      | identName ^^ PropKey
      | NUMBER ^^ { n: Double => PropIx(n) }
  )

  // TODO: strip ``s in result?
  def quasiExpr: Parser[Expression] = (
    QUASI_ALL ^^ { a => Quasi(Left(a)) }
      | QUASI_HEAD ~ expr ~ rep(QUASI_MID ~ expr) ~ QUASI_TAIL ^^ {
        case h ~ e ~ ms ~ t => Quasi(qunpack(h, e, ms, t): _*)
      }
  )

  def memberPostOp: Parser[Expression => Expression] = (
    "[" ~> indexExpr <~ "]" ^^ { ix => e: Expression => Index(e, ix) }
      | "." ~> identName ^^ { id => e: Expression => Get(e, id) }
      | quasiExpr ^^ { q => e: Expression => Tag(e, q) }
  )
  def callPostOp = memberPostOp | args

  // Because Justin and Jessie have no "new" or "super", they don't need
  // to distinguish callExpr from memberExpr. So justin omits memberExpr
  // and newExpr. Instead, in Justin, callExpr jumps directly to
  // primaryExpr and updateExpr jumps directly to callExpr.

  def callExpr = primaryExpr ~ (callPostOp *) ^^ { binary }

  // Restrict index access to number-names, including
  // floating point, NaN, Infinity, and -Infinity.
  def indexExpr: Parser[Expression] = (
    NUMBER ^^ Number
      | "+" ~> unaryExpr ^^ { e => Unary(Positive, e) }
  )

  def args: Parser[Expression => Expression] = "(" ~> repsep(arg, ",") <~! ")" ^^ {
    es => e: Expression => Call(e, es)
  }

  def arg: Parser[Expression] =
    (assignExpr
      | "..." ~> assignExpr ^^ SpreadExpr)

  def updateExpr = callExpr

  def unaryExpr: Parser[Expression] = (
    preOp ~ unaryExpr ^^ { case op ~ e => op(e) }
      | updateExpr
  )

  // No prefix or postfix "++" or "--".
  // No "delete".
  def op1(op: UnOp)(tok: String)(e: Expression) = Unary(op, e)
  def preOp: Parser[Expression => Expression] = (
    "void" ^^ { op1(Void) }
      | "typeof" ^^ { op1(TypeOf) }
      | prePre
  )
  def prePre: Parser[Expression => Expression] = (
      "+" ^^ { op1(Positive) }
      | "-" ^^ { op1(Negative) }
      | "~" ^^ { op1(BitwiseNegate) }
      | "!" ^^ { op1(Not) }
  )

  def powExpr: Parser[Expression] = (
    updateExpr ~ ("**" ~> powExpr) ^^ { case e1 ~ e2 => Binary("**", e1, e2) }
      | unaryExpr
  )

  def binStep(p: String ~ Expression)(lt: Expression): Expression = p match {
    case tok ~ rt => Binary(tok, lt, rt)
  }
  def multExpr: Parser[Expression] =
    powExpr ~ ((
      (("*" | "/" | "%") ~ powExpr) ^^ binStep
    ) *) ^^ { binary }

  def addExpr: Parser[Expression] =
    multExpr ~ (((("+" | "-") ~ multExpr) ^^ binStep) *) ^^ { binary }
  // TODO: shiftExpr
  // TODO: eagerExpr
  def relExpr: Parser[Expression] =
    addExpr ~ ((((relOp ~ addExpr) ^^ binStep) ?) ^^ { _.toList }) ^^ { binary }
  def relOp: Parser[String] = "<" | ">" | "<=" | ">=" | "===" | "!=="
  def andThenExpr: Parser[Expression] =
    relExpr ~ ((("&&" ~ relExpr) ^^ binStep) *) ^^ { binary }
  def orElseExpr: Parser[Expression] =
    andThenExpr ~ ((("||" ~ andThenExpr) ^^ binStep) *) ^^ { binary }
  // TODO: condExpr

  override def assignExpr = orElseExpr
  def expr = log(assignExpr)("expr")
}

class JessieParser extends JustinParser {
  type Body = List[Either[Statement, Declaration]]
  type Bindings = List[(Pattern, Expression)]

  def startProgram: Parser[Program] = phrase(log(moduleBody)("moduleBody"))

  def later: Parser[String] = "~."  // TODO: followed by non-digit

  override def primaryExpr: Parser[Expression] = positioned(
    super.primaryExpr
      | functionExpr
  )

  override def propDef: Parser[Property] = (
    log(methodDef)("methodDef")
    | super.propDef
  )

  // Recognize pre-increment/decrement.
  override def prePre = ("++" ^^ op1(PreIncr) | "--" ^^ op1(PreDecr)) | super.prePre

  // Extend to recognize proposed eventual get syntax,
  // as well as computed indices and postfix increment/decrement.
  override def memberPostOp: Parser[Expression => Expression] = (
    super.memberPostOp
      | "[" ~> assignExpr <~ "]" ^^ {
        ix => e: Expression => Index(e, ix) }
      | later ~> "[" ~> indexExpr <~ "]" ^^ {
        ix => e: Expression => IndexLater(e, ix) }
      | later ~> identName ^^ {
        id => e: Expression => GetLater(e, id) }
  )

  // Extend to recognize proposed eventual send syntax.
  // We distinguish b~.foo(x) from calling b~.foo by a post-parsing pass
  override def callPostOp = (
    super.callPostOp
    | "(" ~> repsep(arg, ",") <~! ")" ^^ { args => e: Expression =>
        Call(e, args)
      }
      | later ~> "(" ~> repsep(arg, ",") <~! ")" ^^ { args => e: Expression =>
        CallLater(e, args)
      }
  )

  def postOp = "++" ^^ op1(PostIncr) | "--" ^^ op1(PostDecr)

  override def assignExpr: Parser[Expression] = (
    arrowFunc
      | functionExpr
    // TODO: lValue postOp
      | (lValue ~ ("=" | assignOp)) ~ assignExpr ^^ { case (lhs ~ op) ~ rhs => Assign(op, lhs, rhs) }
      | super.assignExpr
      | primaryExpr
  )


  // In Jessie, an lValue is only a variable, a computed index-named
  // property (an array element), or a statically string-named
  // property.
  // We allow assignment to statically string-named fields, since it
  // is useful during initialization and prevented thereafter by
  // mandatory tamper-proofing.
  def lValue = (
    primaryExpr ~ ("[" ~> indexExpr <~! "]") ^^ {
      case pe ~ e => Index(pe, e) }
      | primaryExpr ~ (later ~> "[" ~> indexExpr <~! "]") ^^ {
        case pe ~ e => IndexLater(pe, e) }
      | primaryExpr ~ ("." ~> identName) ^^ {
        case pe ~ id => Get(pe, id) }
      | primaryExpr ~ (later ~> "." ~> identName) ^^ {
        case pe ~ id => GetLater(pe, id) }
      | useVar
  )

  def assignOp = ("*=" | "|=" | "%=" | "+=" | "-="
    | "<<=" | ">>=" | ">>>="
    | "&=" | "^=" | "|="
    | "**=")

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

  // TODO: def arm = block
  // TODO: elseArm
  // TODO: breakableStatement

  // Each case branch must end in a terminating statement.
  def terminator: Parser[Statement] = (
    "continue" ~> ((NO_NEWLINE ~> IDENT) ?) <~! ";" ^^ { Continue(_) }
      // TODO: continue ident, break ident
      | "break" ~> ((NO_NEWLINE ~> IDENT) ?) <~! ";" ^^ { Break(_) }
      | "return" ~> ((NO_NEWLINE ~> expr) ?) <~! ";" ^^ { Return(_) }
      | "throw" ~> expr <~! ";" ^^ { Throw(_) }
  )

  def block: Parser[Block] = log("{" ~> body <~ "}")("block") ^^ { Block(_) }
  def body: Parser[Body] = rep(statementItem)
  // declaration must come first, so that PEG will prioritize
  // function declarations over exprStatement. TODO: test
  def statementItem = (
    declaration ^^ { Right(_) }
      | statement ^^ { Left(_) }
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

  // TODO: forOfBinding

  def binding = (
    bindingPattern ~ ("=" ~> assignExpr) ^^ { case p ~ e => (p, e) }
      | defVar ~ ("=" ~> assignExpr) ^^ { case p ~ e => (p, e) }
      | defVar ^^ { case p => (p, Undefined) }
  )

  def bindingPattern: Parser[Pattern] =
    positioned(
      "[" ~> repsep(elementParam, ",") <~ "]" ^^ { MatchArray(_: _*) }
        | "{" ~> repsep(propParam, ",") <~ "}" ^^ { MatchObj(_: _*) }
    )

  def pattern: Parser[Pattern] =
    log(
      positioned(
        bindingPattern
          | defVar
          | "undefined" ^^^ MatchUndefined
          | dataLiteral ^^ MatchData
        // TODO: | HOLE                                  ^^ { _ => PatternHole }
      )
    )("pattern")

  def elementParam = param

  def param: Parser[Pattern] =
    ("..." ~> pattern ^^ Rest
      | defVar ~ ("=" ~> expr) ^^ { case v ~ e => Optional(v, e) }
      | pattern)

  def propParam: Parser[PropParam] = (
    "..." ~> pattern ^^ RestObj
      | (propName <~ ":") ~ pattern ^^ { case k ~ p => MatchProp(k, p) }
      | (IDENT <~ "=") ~ expr ^^ { case id ~ e      => OptionalProp(id, id, e) }
      | IDENT ^^ { id => MatchProp(PropKey(id), Def(id)) }
  )

  // TODO: exprStatement, cantStartExprStatement
  // TODO: terminatedBody, clause, caseLabel


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
  def functionExpr: Parser[Expression] =
    "function" ~> (defVar ?) ~ ("(" ~> repsep(param, ",") <~! ")") ~ block ^^ {
      case n ~ p ~ b => FunctionExpr(n, p, b)
    }

  def functionDecl: Parser[Declaration] =
    "function" ~> defVar ~ ("(" ~> repsep(param, ",") <~! ")") ~ block ^^ {
      case n ~ p ~ b => FunctionDecl(n, p, b)
    }

  def arrowFunc: Parser[Expression] = (
    (arrowParams <~ /* TODO: NO_NEWLINE */ "=>") ~ block ^^ {
      case ps ~ b => Arrow(ps, b)
    }
      | (arrowParams /* TODO: NO_NEWLINE */ <~ "=>") ~ assignExpr ^^ {
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
      | "get" ~> (propName <~ "(" <~! ")") ~ block ^^ {
        case n ~ b => Getter(n, List(), b)
      }
      | "set" ~> propName ~ ("(" ~> param <~! ")") ~ block ^^ {
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
    (bindingPattern <~ "=") ~ expr ^^ { case p ~ e => (p, e) }
      | (defVar <~ "=") ~ expr ^^ { case p ~ e     => (p, e) }
      | defVar ^^ { case Def(id)                           => (Def(id), Use(id)) }
  )

  def useImport: Parser[Expression] = IDENT ^^ { case i => Use(i) }

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
  def exportableExpr = expr
}
