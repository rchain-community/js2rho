/**
  * based on https://github.com/Agoric/TinySES/blob/master/src/tinyses.js
  */

package tinyses  // ISSUE: package com.madmode.tinyses?

import scala.util.matching.Regex
import scala.util.parsing.combinator._

sealed trait Expression

sealed trait DataLiteral extends Expression
sealed trait BooleanLiteral extends DataLiteral
case object Null extends DataLiteral
case object True extends BooleanLiteral
case object False extends BooleanLiteral
case class Number(value: Double) extends DataLiteral
case class StrLit(value: String) extends DataLiteral

case class Use(id: String) extends Expression

case class ArrayExpr(items: Expression*) extends Expression {
  override def toString() = items.mkString("[", ", ", "]")
}
case class ObjectExpr(properties: Property*) extends Expression {
  override def toString() = properties.mkString("{", ", ", "}")
}
case class FunctionExpr(n: Option[Pattern], params: List[Pattern], block: Block) extends Expression
case object ExprHole extends Expression  // ISSUE: really?
case class Quasi(pieces: Either[String, Expression]*) extends Expression
case class Get(e: Expression, id: String) extends Expression
case class Index(e: Expression, ix: Expression) extends Expression
case class Call(e: Expression, args: List[Expression]) extends Expression
case class Tag(e: Expression, q: Expression) extends Expression
case class GetLater(e: Expression, id: String) extends Expression
case class IndexLater(e: Expression, ix: Expression) extends Expression
case class CallLater(e: Expression, args: List[Expression]) extends Expression

case class Assign(op: String, lhs: Expression, rhs: Expression) extends Expression // TODO: AssignOp
case class Arrow(ps: List[Pattern], b: Block) extends Expression
case class Lambda(ps: List[Pattern], e: Expression) extends Expression

case class Unary(op: UnOp, e: Expression) extends Expression
sealed trait UnOp
case object Void extends UnOp
case object TypeOf extends UnOp
case object Positive extends UnOp
case object Negative extends UnOp
case object Not extends UnOp

case class SpreadExpr(tail: Expression) extends Expression // ISSUE: really?

sealed trait Property
case class Prop(k: PropName, e: Expression) extends Property {
  override def toString() = k.toString + ": " + e.toString
}
case class SpreadObj(e: Expression) extends Property
case class MethodDef(n: PropName, p: List[Pattern], b: Block) extends Property
case class Getter(n: PropName, p: List[Pattern], b: Block) extends Property
case class Setter(n: PropName, p: List[Pattern], b: Block) extends Property

sealed trait PropName
case class PropKey(k: String) extends PropName {
  override def toString() = k
}
case class PropIx(ix: Double) extends PropName {
  override def toString() = ix.toString
}

case class Rest(p: Pattern) extends Pattern  // ISSUE: really??
case class Optional(v: Pattern, e: Expression) extends Pattern  // ISSUE: really??

sealed trait PropParam
case class RestObj(p: Pattern) extends PropParam
case class MatchProp(k: PropName, p: Pattern) extends PropParam
case class OptionalProp(k: String, k2: String, e: Expression) extends PropParam // ISSUE: k2? types?

sealed trait Pattern
case class Def(id: String) extends Pattern
case class MatchData(d: DataLiteral) extends Pattern
case class MatchArray(ps: Pattern*) extends Pattern
case class MatchObj(ps: PropParam*) extends Pattern
case object PatternHole extends Pattern


sealed trait Statement
case class ExprStmt(e: Expression) extends Statement
case class Block(body: List[Either[Statement, Declaration]]) extends Statement


sealed trait Declaration
case class FunctionDecl(n: Pattern, p: List[Pattern], b: Block) extends Declaration

// ISSUE: use JavaTokenParsers instead?
class JSONTokens extends RegexParsers {
  override def skipWhitespace = true
  override val whiteSpace: Regex = raw"\s+".r
  def NUMBER: Parser[Double] = raw"-?\d+(?:\.\d+)?(?:[eE]-?\d+)?".r ^^ { n => n.toDouble }
  val UCODE_RE = raw"\\u[\da-fA-F]{4}"
  // Note no \' (escaped single quote) in accord with JSON.
  // TODO: val CHAR_RE = r"[^\"\\]|\\"|\\\\|\\/|\\b|\\f|\\n|\\r|\\t|" // TODO: + UCODE_RE
  // TODO: def STRING: Parser[String] = s"\"${CHAR_RE}*\"".r
  def STRING: Parser[String] = "\"[^\"]*\"".r
  def IDENT: Parser[String] = raw"[a-zA-Z_$$][\w$$]*".r
}

class TinySESParser extends JSONTokens {
  def binary[R](p: Expression ~ R): Expression = p match { case left ~ _rights => left } // TODO: rights.foldLeft(case (prev, List(op, right)) => List(op,prev,right), left)

  def qunpack(h: String, /*ms: Option[(Expression, List[String ~ Expression]],*/ t: String): Seq[Either[String, Expression]] = ???  /** TODO  {
    val result = List(h)
    if (ms.size == 1) {
      val List([m, pairs]) = ms
      result.push(m)
      for (let (q,e) <- pairs) {
        result.push(q,e)
      }
    }
    result.push(t)
    return result
  }
*/

  type Body = List[Either[Statement, Declaration]]

  // TODO: def start: Parser[_] = phrase(body)

  // TODO: Error if whitespace includes newline
  def NO_NEWLINE: Parser[String] = "TODO@@"

  // TODO: quasiliterals aka template literals
  def HOLE: Parser[String] = "HOLE TODO@@"
  def QUASI_ALL: Parser[String] = "@@TODO"
  def QUASI_HEAD: Parser[String] = "@@TODO"
  def QUASI_MID: Parser[String] = "@@TODO"
  def QUASI_TAIL: Parser[String] = "@@TODO"


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
      | "else" | "export"
      | "finally" | "for" | "function"
      | "if" | "import"
      | "return"
      | "switch"
      | "throw" | "try" | "typeof"
      | "void"
      | "while"
  )

  // Unused by TinySES but enumerated here, in order to omit them
  // from the IDENT token.
  def RESERVED_KEYWORD: Parser[String] = (
    "class"
      | "delete" | "do"
      | "extends"
      | "in" | "instanceof"
      | "new"
      | "super"
      | "this"
      | "var"
      | "with"
      | "yield"
  )

  def FUTURE_RESERVED_WORD: Parser[String] = (
    "await" | "enum"
      | "implements" | "package" | "protected"
      | "interface" | "private" | "public"
  )

  def dataLiteral: Parser[DataLiteral] =  (
    "null"        ^^ { _ => Null }
      | "false"   ^^ { _ => False }
      | "true"    ^^ { _ => True }
      | NUMBER    ^^ Number
      | STRING    ^^ StrLit
  )

  def identName: Parser[String] = IDENT | RESERVED_WORD
  def useVar: Parser[Expression] = IDENT                    ^^ Use
  def defVar: Parser[Pattern] = IDENT                    ^^ Def

  // For most identifiers that ES2017 treats as IDENT but recognizes
  // as pseudo-keywords in a context dependent manner, TinySES simply makes
  // keywords. However, this would be too painful for "get" and
  // "set", so instead we ???use our parser-generator's support syntactic
  // predicates. TODO: Is it really too painful? Try it.
  // identGet ::= IDENT ^^ { id => if (id == "get") { id } else { FAIL }}
  // identSet ::= IDENT ^^ { id => if (id == "set") { id } else { FAIL }}
  def identGet: Parser[String] = "get"
  def identSet: Parser[String] = "set"

  // TinySES primaryExpr does not include "this", ClassExpression,
  // GeneratorExpression, or RegularExpressionLiteral.
  def primaryExpr: Parser[Expression] = (
    dataLiteral
    | "[" ~> repsep(arg, ",")  <~ "]"   ^^ { args => ArrayExpr(args:_*) }
  | "{" ~> repsep(prop, ",") <~ "}"   ^^ { ps => ObjectExpr(ps:_*) }
  | functionExpr
  | quasiExpr
  | "(" ~> expr <~ ")"
  | useVar
  | HOLE               ^^ { _ => ExprHole }
  )

  def pattern :Parser[Pattern] = (
    dataLiteral ^^ MatchData
    | "[" ~> repsep(param, ",") <~ "]"      ^^ { MatchArray(_:_*) }
    | "{" ~> repsep(propParam, ",") <~ "}"  ^^ { MatchObj(_:_*) }
    | defVar
    | HOLE                                  ^^ { _ => PatternHole }
				  )

  def arg: Parser[Expression] = (
    "..." ~> expr                                    ^^ SpreadExpr
      | expr)

  def param :Parser[Pattern] = (
    "..." ~> pattern                ^^ Rest
      | defVar ~ ("=" ~> expr)        ^^ { case v ~ e => Optional(v, e) }
      | pattern)

  def prop: Parser[Property] = (
    "..." ~> expr                 ^^ SpreadObj
      | (propName <~ ":") ~ expr  ^^ { case k ~ e => Prop(k, e) }
      | methodDef
      | IDENT                     ^^ { id => Prop(PropKey(id), Use(id)) }
  )

  def propParam :Parser[PropParam] = (
    "..." ~> pattern              ^^ RestObj
      | (propName <~ ":") ~ pattern ^^ { case k ~ p => MatchProp(k, p) }
      | (IDENT <~ "=") ~ expr       ^^ { case id ~ e => OptionalProp(id, id, e) }
      | IDENT                       ^^ { id => MatchProp(PropKey(id), Def(id)) }
				      )
  // No computed property name
  def propName: Parser[PropName] =  (
    (identName | STRING)  ^^ PropKey
      | NUMBER            ^^ { n: Double => PropIx(n) }
  )

  def quasiExpr :Parser[Expression] = (
    QUASI_ALL             ^^ { a => Quasi(Left(a))}
      | QUASI_HEAD ~ (((expr ~ (QUASI_MID ~ expr)*)?) ~ QUASI_TAIL) ^^ {
        case h ~ (ms ~ t)  => Quasi(qunpack(h, t):_*) }
				 )
  def later: Parser[String] = NO_NEWLINE ~> "!"

  // No "new", "super", or MetaProperty. Without "new" we don't need
  // separate MemberExpr and CallExpr productions.
  // Recognize b!foo(x) as distinct from calling b!foo post-parse.
  def postExpr :Parser[Expression] = primaryExpr ~ (postOp*) ^^ { binary }
  def postOp :Parser[Expression => Expression] = (
    "." ~> identName                   ^^ { id => e: Expression => Get(e, id) }
      | "[" ~> indexExpr <~ "]"          ^^ { ix => e: Expression => Index(e, ix) }
      | "(" ~> repsep(arg, ",") <~ ")"   ^^ { args => e: Expression => Call(e, args) }
      | quasiExpr                        ^^ { q => e: Expression => Tag(e, q) }
      | later ~> identName               ^^ { id => e: Expression => GetLater(e, id) }
      | later ~> "[" ~> indexExpr <~ "]" ^^ { ix => e: Expression => IndexLater(e, ix) }
      | later ~> "(" ~> repsep(arg, ",") <~ ")"  ^^ { args => e: Expression => CallLater(e, args) }
  )
  // Omit ("delete" fieldExpr) to avoid mutating properties
  def preExpr :Parser[Expression] = (
    preOp ~ preExpr                      ^^ { case op ~ e => op(e) }
      | postExpr
  )
  // No prefix or postfix "++" or "--".
  // No "delete". No bitwise "~".
  def preOp:Parser[Expression => Expression] = (
    "void" ^^ { _ => e: Expression => Unary(Void, e) }
      | "typeof" ^^ { _ => e:Expression => Unary(TypeOf, e) }
      | "+" ^^ { _ => e:Expression => Unary(Positive, e) }
      | "-" ^^ { _ => e:Expression => Unary(Negative, e) }
      | "!" ^^ { _ => e:Expression => Unary(Not, e) }
  )

  // Restrict index access to number-names, including
  // floating point, NaN, Infinity, and -Infinity.
  def indexExpr: Parser[Expression] = "+" ~> preExpr       ^^ { e  => Unary(Positive, e) }

  // No bitwise operators, "instanceof", "in", "==", or "!=".  Unlike
  // ES8, none of the relational operators (including equality)
  // associate. To help readers, mixing relational operators always
  // requires explicit parens.
  // TODO: exponentiation "**" operator.
  def multExpr: Parser[Expression] = preExpr ~ ((("*" | "/" | "%") ~ preExpr)*)      ^^ { binary }

  def addExpr: Parser[Expression] = multExpr ~ ((("+" | "-") ~ multExpr)*)      ^^ { binary }
  def relExpr: Parser[Expression] = addExpr ~ ((relOp ~ addExpr)?)            ^^ binary
  def relOp :Parser[String] = "<" | ">" | "<=" | ">=" | "===" | "!=="
  def andThenExpr: Parser[Expression] = relExpr ~ (("&&" ~ relExpr)* )          ^^ binary
  def orElseExpr :Parser[Expression] = andThenExpr ~ (("||" ~> andThenExpr)*)    ^^ binary
  // No trinary ("?:") expression
  // No comma expression, so assignment expression is expr.
  // TODO: Need to be able to write (1,array[i])(args), which
  // either requires that we readmit the comma expression, or
  // that we add a weird special case to the grammar.
  def expr: Parser[Expression] = (
    lValue ~ (assignOp ~ expr)      ^^ { case lv ~ (op ~ rv) => Assign(op, lv, rv) }
      | arrow
      | orElseExpr
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
    primaryExpr ~ ("[" ~> indexExpr <~ "]") ^^ { case pe ~ e => Index(pe, e) }
      | primaryExpr ~ (later ~> "[" ~> indexExpr <~ "]") ^^ { case pe ~ e => IndexLater(pe, e) }
  )
  def fieldExpr: Parser[Expression] = (
    primaryExpr ~ ("." ~> identName)                 ^^ { case pe ~ id => Get(pe, id) }
      | primaryExpr ~ (later ~> "." ~> identName)    ^^ { case pe ~ id => GetLater(pe, id) }
      | elementExpr
  )
  // No bitwise operators
  def assignOp = "=" | "*=" | "|=" | "%=" | "+=" | "-="
  // The expr form must come after the block form, to make proper use
  // of PEG prioritized choice.
  def arrow: Parser[Expression] = (
    params ~ (NO_NEWLINE ~> "=>" ~> block)       ^^ { case ps ~ b => Arrow(ps, b) }
      | params ~ (NO_NEWLINE ~> "=>" ~> expr)        ^^ { case ps ~ e => Lambda(ps, e) }
  )
  def params :Parser[List[Pattern]] = (
    IDENT                                        ^^ { id => List(Def(id)) }
      | "(" ~> repsep(param, ",") <~ ")"
  )

  // No "var", empty statement, "with", "do/while", or "for/in". None
  // of the insane variations of "for". Only blocks are accepted for
  // flow-of-control statements.
  // The expr production must go last, so PEG's prioritized choice will
  // interpret {} as a block rather than an expression.
  def statement :Parser[Statement] = (
    block
  /** TODO
      | "if" ~> ("(" ~> expr <~ ")") ~ block ~ ("else" ~> block)? ^^ { case c ~ t ~ e => If(c, t, e) }
      | "for" ~> "(" ~> declaration ~> expr? ~ (";" ~> expr?) ~ ")" ~ block    ^^ { case d ~ c ~ i ~ b => For(d, c, i, b) }
      | "for" ~> "(" ~> declOp ~ binding ~ ("of" ~> expr <~ ")") ~ block       ^^ { case d ~ e ~ b => ForOf(d, e, b) }
      | "while" ~> ("(" ~> expr <~ ")") ~ block               ^^ { case c ~ b => While(c, b) }
      | "switch" ~> ("(" ~> expr <~ ")") ~ ("{" ~> (branch*) <~ "}") ^^ { case e ~ bs => Switch(e, bs) }
      | (IDENT <~ ":") ~ statement                           ^^ { case label ~ stat => Label(label, stat) }
      | "try" ~> block ~ catcher? ~ finalizer?                 ^^ { case b ~ c ~ f => Try(b, c, f) }
      | terminator
      | "debugger" <~ ";"                                    ^^ { Debugger }
  */
      | expr <~ ";"                                          ^^ { e => ExprStmt(e) }
  )
  // Each case branch must end in a terminating statement.
  /** TODO
  def terminator = (
    "return" ~> ((NO_NEWLINE ~> expr)?) <~ ";"                    ^^ { Return(_) }
      | "break" ~> ((NO_NEWLINE ~> IDENT)?) <~ ";"                ^^ { Break(_) }
      | "continue" ~> ((NO_NEWLINE ~> IDENT)?) <~ ";"             ^^ { Continue(_) }
      | "throw" ~> expr <~ ";"                                  ^^ { Throw(_) }
  )
  */
  // No "class" declaration.
  // No generator, async, or async iterator function.
  def declaration: Parser[Declaration] = (
  /** TODO
    declOp ~ repsep(binding, ",") <~ ";"                        ^^ { case op ~ decls => op(decls) }
      | */ functionDecl
  )
  /** TODO
  def declOp = "const" | "let"
  // Initializer is mandatory
  def binding = pattern ("=" ~> expr)                        ^^ { case p ~ e => Bind(p, e) }
  def catcher = "catch" ~> ("(" ~> pattern <~ ")") ~ block   ^^ { case p ~ b => Catch(p, b) }
  def finalizer = "finally" ~> block                         ^^ { Finally(_) }
  def branch = caseLabel+ ~ ("{" ~> body ~ terminator ~ "}") ^^ { case cs ~ b ~ t =>
    Branch(cs, b ++ List(t)) }
  def caseLabel = (
  "case" ~> expr <~ ":"                                  ^^ { Case(_) }
    | "default" ~ ":"                                    ^^ { Default(_) }
  )
    **/

  def block :Parser[Block] = "{" ~> body <~ "}" ^^ {Block(_)}
  def body :Parser[Body] = rep(statement ^^ { Left(_) } | declaration ^^ { Right(_) })
  def functionExpr: Parser[Expression] = "function" ~> (defVar?) ~ ("(" ~> params <~ ")") ~ block   ^^ {
    case n ~ p ~ b => FunctionExpr(n, p, b) }

  def functionDecl :Parser[Declaration] = "function" ~> defVar ~ ("(" ~> params <~ ")") ~ block ^^ {
    case n ~ p ~ b => FunctionDecl(n, p, b) }
  def methodDef : Parser[Property] = (
    propName ~ ("(" ~> params <~ ")") ~ block   ^^ { case n ~ p ~ b => MethodDef(n, p, b) }
      | identGet ~> (propName <~ "(" <~ ")") ~ block ^^ { case n ~ b => Getter(n, List(), b) }
      | identSet ~> propName ~ ("(" ~> param <~ ")") ~  block ^^ { case n ~ p ~ b => Setter(n, List(p), b) }
  )
}

object TestSimpleParser extends TinySESParser {
    def main(args: Array[String]) = {
        parse(primaryExpr, "[1, true, false, {x} ]") match {
            case Success(matched,_) => println(matched)
            case Failure(msg,_) => println("FAILURE: " + msg)
            case Error(msg,_) => println("ERROR: " + msg)
        }
    }
}
