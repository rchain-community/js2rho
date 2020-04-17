package tinyses  // ISSUE: package com.madmode.tinyses?

import escalima.{ECMAScript, ast}

object JsParseEx1 {
  def main(args: Array[String]) = {
    val parser = new ECMAScript
    val source = "test(id)"
    val program = parser.parseModule(source)

    program match {
      case ast.Program(Seq(stmt)) =>
        stmt match {
          case ast.ExpressionStatement(ast.CallExpression(_, Seq(arg))) =>
            println(arg.toJSON)
        }
    }
  }
}
