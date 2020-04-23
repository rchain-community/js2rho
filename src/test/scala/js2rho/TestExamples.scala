package js2rho

import java.net.URI

import scala.io.Source
import org.scalatest.FlatSpec

class WalnutPatternsTest extends FlatSpec {
  val notaryRes = getClass.getResource("/examples/notary.js").toURI()
  lazy val notaryJs = Source.fromFile(notaryRes).getLines.mkString("\n")
  lazy val jp = new JessieParser()
  lazy val notaryAST = jp.parse(jp.start, notaryJs)

  "quasi regex" should "be right" in {
    assert("`abc`".matches(QuasiParser.all_pat.regex))
    assert("`abc${".matches(QuasiParser.head_pat.regex))
    assert("}abc${".matches(QuasiParser.mid_pat.regex))
    assert("}abc".matches(QuasiParser.tail_pat.regex))
    // TODO: handle `Object not vouchable: $obj`
  }

  "notaryRes" should "be a URI" in {
    (notaryRes, notaryJs) match {
      case (_: URI, _: String) => null
      case _                   => assert(false)
    }
  }

  "identifier" should "parse" in {
    jp.parse(jp.IDENT, "abc") match {
      case jp.Success(id, _) => assert(id == "abc")
      case _                 => assert(false)
    }
  }

  "notary.js" should "parse" in {
    notaryAST match {
      case jp.Success(ast, _) => ast
      case _                  => assert(false)
//    case Failure(msg,_) => println("FAILURE: " + msg)
//    case Error(msg,_) => println("ERROR: " + msg)
    }
  }
}
