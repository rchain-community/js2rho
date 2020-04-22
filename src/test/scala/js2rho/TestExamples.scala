package js2rho

import scala.io.Source

trait Rd {
  def contents(): String
  def resolve(there: String): Rd
}

class SourceRd(path: String) extends Rd {
  def contents() = Source.fromFile(path).getLines.mkString("\n")
  def resolve(there: String) = new SourceRd(path + '/' + there)
}

object TestExamples {
  def main(args: Array[String]) {
    val exDir = new SourceRd("examples")
    val s1: String = exDir.resolve("mint.js").contents()
    test1(s1)
  }

  val js = new JessieParser

  def test1(src: String) = js.parse(js.phrase(js.start), src) match {
    case js.Success(matched, _) => println(matched)
    case js.Failure(msg, _)     => println("FAILURE: " + msg)
    case js.Error(msg, _)       => println("ERROR: " + msg)
  }
}
