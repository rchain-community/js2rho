/**
  * based on https://github.com/Agoric/TinySES/blob/master/src/tinyses.js
  */

package com.madmode.tinyses

import scala.util.parsing.combinator._

sealed trait Pattern

sealed trait Expression

//type Body = List[Either[Statement, Declaration]]
case class Block(body: Expression)
sealed trait DataLiteral extends Expression
sealed trait BooleanLiteral extends DataLiteral
case object Null extends DataLiteral
case object True extends BooleanLiteral
case object False extends BooleanLiteral
case class Number(value: Double) extends DataLiteral
case class StrLit(value: String) extends DataLiteral

case class Def(id: String) extends Pattern
case class Use(id: String) extends Expression

case class ArrayExpr(items: Expression*) extends Expression
case class ObjectExpr(properties: Property*) extends Expression
case class Spread(items: Expression*) extends Expression
case class FunctionExpr(n: Maybe[Def], params: List[Param], block: Block)
sealed trait Property
case class Prop(k: PropName, e: Expression) extends Property
case class SpreadObj(e: Expression) extends Property

sealed trait PropName
case class PropKey(k: String) extends PropName
case class PropIx(ix: Double) extends PropName

sealed trait Param
case class Rest(p: Pattern) extends Param
case class Optional(v: Def, e: Expression) extends Param

// ISSUE: use JavaTokenParsers instead?
class JSONTokens extends RegexParsers {
  override def skipWhitespace = true
  override val whiteSpace = raw"\s+".r
  def NUMBER: Parser[Double] = raw"-?\d+(?:\.\d+)?(?:[eE]-?\d+)?".r ^^ { n => n.toDouble }
  val UCODE_RE = raw"\\u[\da-fA-F]{4}"
  // Note no \' (escaped single quote) in accord with JSON.
  // TODO: val CHAR_RE = r"[^\"\\]|\\"|\\\\|\\/|\\b|\\f|\\n|\\r|\\t|" // TODO: + UCODE_RE
  // TODO: def STRING: Parser[String] = s"\"${CHAR_RE}*\"".r
  def STRING: Parser[String] = "\"[^\"]*\"".r
  def IDENT: Parser[String] = raw"[a-zA-Z_$$][\w$$]*".r
}

class TinySESParser extends JSONTokens {
  // TODO: def start: Parser[_] = phrase(body)

  // TODO: Error if whitespace includes newline
  // def NO_NEWLINE

  // TODO: quasiliterals aka template literals
  //  QUASI_ALL ::= ${() => FAIL};
  //  QUASI_HEAD ::= ${() => FAIL};
  //  QUASI_MID ::= ${() => FAIL};
  //  QUASI_TAIL ::= ${() => FAIL};

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
      | NUMBER    ^^ { Number(_) }
      | STRING    ^^ { StrLit(_) }
  )

  def identName = IDENT | RESERVED_WORD
  def useVar = IDENT                    ^^ { Use(_) }
  def defVar = IDENT                    ^^ { Def(_) }

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
    | HOLE               ^^ { ExprHole(_) }
  )
  def pattern :Parser[Pattern] = (
    dataLiteral ^^ { MatchData(_) }
    | "[" ~> repsep(param, ",") <~ "]"      ^^ { MatchArray(_) }
    | "{" ~> repsep(propParam, ",") <~ "}"  ^^ { MatchObj(_) }
    | defVar
    | HOLE                                  ^^ { PatternHole(_) }
				  )

  def arg: Parser[Expression] = (
    "..." ~> expr                                    ^^ { Spread(_) }
      | expr)

  def param :Parser[Param] = (
    "..." ~> pattern                ^^ { Rest(_) }
      | defVar ~ ("=" ~> expr)        ^^ { case v ~ e => Optional(v, e) }
      | pattern)

  def prop = (
    "..." ~> expr                 ^^ { SpreadObj(_) }
      | (propName <~ ":") ~ expr  ^^ { case k ~ e => Prop(k, e) }
      | methodDef
      | IDENT                     ^^ { id => Prop(PropKey(id), Use(id)) }
  )

  def propParam :Parser[PropParam] = (
    "..." ~> pattern              ^^ { RestObj(_) }
      | (propName <~ ":") ~ pattern ^^ { case k ~ p => MatchProp(k, p) }
      | (IDENT <~ "=") ~ expr       ^^ { case id ~ e => OptionalProp(id, id, e) }
      | IDENT                       ^^ { id => MatchProp(id, id) }
				      )
  // No computed property name
  def propName: Parser[PropName] =  (
    (identName | STRING)  ^^ { PropKey(_) }
      | NUMBER            ^^ { n: Double => PropIx(n) }
  )

  def quasiExpr :Parser[Expr] = (
    QUASI_ALL             ^^ { Quasi(_)}
      | QUASI_HEAD ~ (expr (QUASI_MID ~ expr)*)? ~ QUASI_TAIL ^^ {
        case h ~ ms ~ t  => Quasi(qunpack(h, ms, t)) }
				 )
  def later: Parser[String] = NO_NEWLINE >~ "!"

  // No "new", "super", or MetaProperty. Without "new" we don't need
  // separate MemberExpr and CallExpr productions.
  // Recognize b!foo(x) as distinct from calling b!foo post-parse.
  def postExpr :Parser[Expr] = primaryExpr ~ postOp* ^^ binary
  def postOp :Parser[Expr => Expr] = (
    "." ~> identName                   ^^ { id => e => Get(e, id) }
      | "[" ~> indexExpr <~ "]"          ^^ { ix => e => Index(e, ix) }
      | "(" ~> repsep(arg, ",") <~ ")"   ^^ { args => e => Call(e, args) }
      | quasiExpr                        ^^ { q => e => Tag(e, q) }
      | later ~> identName               ^^ { id => e => GetLater(e, id) }
      | later ~> "[" ~> indexExpr <~ "]" ^^ { ix => e => IndexLater(e, ix) }
      | later ~> "(" ~> repsep(arg, ",") <~ ")"  ^^ { args => e =>
	CallLater(e, args) }
  )
  // Omit ("delete" fieldExpr) to avoid mutating properties
  def preExpr :Parser[Expr] = (
    preOp ~ preExpr                      ^^ { case op ~ e => op(e) }
      | postExpr
  )
  // No prefix or postfix "++" or "--".
  // No "delete". No bitwise "~".
  def preOp:Parser[Expr => Expr] = (
    "void" ^^ { Void(_) }
      | "typeof" ^^ { TypeOf(_) }
      | "+" ^^ { Positive(_) }
      | "-" ^^ { Negative(_) }
      | "!" ^^ { Not(_) }
  )
  def addExpr = multExpr ~ (("+" | "-") multExpr)*      ^^ binary
  def relExpr = addExpr ~ (relOp ~ addExpr)?            ^^ binary
  def relOp :Parser[String] = "<" | ">" | "<=" | ">=" | "===" | "!=="
  def andThenExpr = relExpr ("&&" ~ relExpr)*           ^^ binary
  def orElseExpr :Parser[Expression] = andThenExpr ~ ("||" ~> andThenExpr)*    ^^ binary
  // No trinary ("?:") expression
  // No comma expression, so assignment expression is expr.
  // TODO: Need to be able to write (1,array[i])(args), which
  // either requires that we readmit the comma expression, or
  // that we add a weird special case to the grammar.
  def expr = (
    lValue ~ assignOp ~ expr      ^^ { case lv ~ op ~ rv => op(lv, rv) }
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
      | primaryExpr (later ~> "[" ~> indexExpr <~ "]") ^^ { case pe ~ e => IndexLater(pe, e) }
  )
  def fieldExpr = (
    primaryExpr ~ ("." ~> identName)                 ^^ { case pe ~ id => Get(pe, id) }
      | primaryExpr ~ (later ~> "." ~> identName)    ^^ { case pe ~ id => GetLater(pe, id) }
      | elementExpr
  )
  // No bitwise operators
  def assignOp :Parser[String] = "=" | "*=" | "|=" | "%=" | "+=" | "-="
  // The expr form must come after the block form, to make proper use
  // of PEG prioritized choice.
  def arrow = (
    params ~ (NO_NEWLINE ~> "=>" ~> block)       ^^ { case ps ~ b => Arrow(ps, b) }
      | params ~(NO_NEWLINE ~> "=>" expr)        ^^ { case ps ~ e => Lambda(ps, e) }
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
      | "if" ~> ("(" ~> expr <~ ")") ~ block ~ ("else" ~> block)? ^^ { case c ~ t ~ e => If(c, t, e) }
      | "for" ~> "(" ~> declaration ~> expr? ~ (";" ~> expr?) ~ ")" ~ block    ^^ { case d ~ c ~ i ~ b => For(d, c, i, b) }
      | "for" ~> "(" ~> declOp ~ binding ~ ("of" ~> expr <~ ")") ~ block       ^^ { case d ~ e ~ b => ForOf(d, e, b) }
      | "while" ~> ("(" ~> expr <~ ")") ~ block               ^^ { case c ~ b => While(c, b) }
      | "switch" ~> ("(" ~> expr <~ ")") ~ ("{" ~> (branch*) <~ "}") ^^ { case e ~ bs => Switch(e, bs) }
      | (IDENT <~ ":") ~ statement                           ^^ { case label ~ stat => Label(label, stat) }
      | "try" ~> block ~ catcher? ~ finalizer?                 ^^ { case b ~ c ~ f => Try(b, c, f) }
      | terminator
      | "debugger" <~ ";"                                    ^^ { Debugger }
      | expr <~ ";"                                          ^^ { e => e }
  )
  // Each case branch must end in a terminating statement.
  def terminator = (
    "return" ~> ((NO_NEWLINE ~> expr)?) <~ ";"                    ^^ { Return(_) }
      | "break" ~> ((NO_NEWLINE ~> IDENT)?) <~ ";"                ^^ { Break(_) }
      | "continue" ~> ((NO_NEWLINE ~> IDENT)?) <~ ";"             ^^ { Continue(_) }
      | "throw" ~> expr <~ ";"                                  ^^ { Throw(_) }
  )
  // No "class" declaration.
  // No generator, async, or async iterator function.
  def declaration = (
    declOp ~ repsep(binding, ",") <~ ";"                        ^^ { case op ~ decls => op(decls) }
      | functionDecl
  )
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

  def block :Parser[Block] = "{" ~> body <~ "}" ^^ {Block(_)}
  def body :Parser[Body] = rep(statement | declaration)
  def functionExpr = "function" ~> defVar? ("(" ~> params <~ ")") ~ block   ^^ {
    case n ~ p ~ b => FunctionExpr(n, p, b) }

  def functionDecl :Parser[Declaration] = "function" ~> defVar ("(" ~> params <~ ")")  block ^^ {
    case n ~ p ~ b => FunctionDecl(n, p, b) }
  def methodDef : Parser[_] = (
    propName ~ ("(" ~> params <~ ")") ~ block   ^^ { case n ~ p ~ b => MethodDef(n, p, b) }
      | identGet ~ (propName <~ "(" <~ ")") ~ block ^^ { case n ~ b => Getter(n, List(), b) }
      | identSet ~ propName ~ ("(" ~> param <~ ")") ~  block ^^ { case n ~ p ~ b => Setter(n, List(p), b) }
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
