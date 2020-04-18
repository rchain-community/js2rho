package js2rho

import escalima.{ECMAScript, ast}
import scala.io.Source

object TestExamples {
  def main(args: Array[String]) {
    val s1: String = Source.fromFile("examples/mint.js").getLines.mkString("\n")
    val parser = new ECMAScript()
    val x = parser.parseModuleToJson(s1)
    println(x)
  }
}
