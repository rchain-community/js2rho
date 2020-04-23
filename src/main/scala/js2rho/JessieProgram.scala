package js2rho

import scala.util.parsing.input.Positional

sealed trait Expression extends Positional

sealed trait DataLiteral extends Expression
sealed trait BooleanLiteral extends DataLiteral
case object Null extends DataLiteral
case object True extends BooleanLiteral
case object False extends BooleanLiteral
case class Number(value: Double) extends DataLiteral
case class StrLit(value: String) extends DataLiteral

case object Undefined extends Expression
case class Use(id: String) extends Expression

case class ArrayExpr(items: Expression*) extends Expression {
  override def toString() = items.mkString("[", ", ", "]")
}
case class ObjectExpr(properties: Property*) extends Expression {
  override def toString() = properties.mkString("{", ", ", "}")
}
case class FunctionExpr(n: Option[Pattern], params: List[Pattern], block: Block) extends Expression
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

case class Binary(op: String, lt: Expression, rt: Expression) extends Expression  // TODO: sum type for BinaryOp

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

sealed trait Pattern extends Positional
case class Def(id: String) extends Pattern
case class MatchData(d: DataLiteral) extends Pattern
case class MatchArray(ps: Pattern*) extends Pattern
case class MatchObj(ps: PropParam*) extends Pattern
// TODO: case object PatternHole extends Pattern


sealed trait Statement extends Positional
case class ExprStmt(e: Expression) extends Statement
case class Block(body: List[Either[Statement, Declaration]]) extends Statement
case class Return(e: Option[Expression]) extends Statement
case class Break(e: Option[String]) extends Statement
case class Continue(e: Option[String]) extends Statement
case class Throw(e: Expression) extends Statement


sealed trait Declaration extends Positional
case class Const(bindings: List[(Pattern, Expression)]) extends Declaration
case class Let(bindings: List[(Pattern, Expression)]) extends Declaration
case class FunctionDecl(n: Pattern, p: List[Pattern], b: Block) extends Declaration

sealed trait ModuleDeclaration extends Positional
sealed trait ModuleSpecifier extends Positional
case class ImportDeclaration(specifiers: List[ModuleSpecifier], source: StrLit) extends ModuleDeclaration
case class ImportSpecifier(local: Identifier, imported: Identifier) extends ModuleSpecifier
case class ImportDefaultSpecifier(local: Identifier) extends ModuleSpecifier
case class ImportNamespaceSpecifier(local: Identifier) extends ModuleSpecifier

case class ExportDefaultDeclaration(e: Expression) extends ModuleDeclaration

case class Identifier(name: String) extends Positional  // estree style rather than Agoric / Jessie style

case class Program(body: List[Either[Statement, ModuleDeclaration]]) extends Positional