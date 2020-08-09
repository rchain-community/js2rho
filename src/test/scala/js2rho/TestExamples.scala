package js2rho

import java.net.URI

import scala.io.Source
import org.scalatest.FlatSpec

class WalnutPatternsTest extends FlatSpec {
  lazy val jp = new JessieParser()

  def parse(res: java.net.URL): jp.ParseResult[_] = {
    val js = Source.fromFile(res.toURI()).getLines.mkString("\n")
    return jp.parse(jp.startProgram, js)
  }

  "identifier" should "parse" in {
    jp.parse(jp.IDENT, "abc") match {
      case jp.Success(id, _) => assert(id == "abc")
      case _                 => assert(false)
    }
  }

  "hello_world.js" should "parse" in {
    parse(getClass.getResource("/examples/hello_world.js")) match {
      case jp.Success(ast, _) => ast
      case jp.Failure(msg,_)  => {
        println("FAILURE: " + msg)
        assert(false);
      }
      case jp.Error(msg,_)    => {
        println("ERROR: " + msg)
        assert(false);
      }
      case _                  => assert(false)
    }
  }

  "quasi regex" should "be right" in {
    assert("`abc`".matches(QuasiParser.all_pat.regex))
    assert("`abc${".matches(QuasiParser.head_pat.regex))
    assert("}abc${".matches(QuasiParser.mid_pat.regex))
    assert("}abc`".matches(QuasiParser.tail_pat.regex))
    assert("}`".matches(QuasiParser.tail_pat.regex))
    // TODO: handle `Object not vouchable: $obj`
  }

  "quasiExpr" should "parse as Expression" in {
    jp.parse(jp.expr, "`Object not vouchable: ${obj}`") match {
      case jp.Success(Quasi(Left(_), Right(_), Left("`")), _) => ()
      case jp.Failure(msg,_)  => {
        println("FAILURE: " + msg)
        assert(false);
      }
      case jp.Error(msg,_)    => {
        println("ERROR: " + msg)
        assert(false);
      }
      case _                  => assert(false)
    }
  }

  lazy val iddbAST = parse(getClass.getResource("/examples/iddb.js"))

  "iddb.js" should "parse" in {
    iddbAST match {
      case jp.Success(ast, _) => ast
      case jp.Failure(msg,_)  => {
        println("FAILURE: " + msg)
        assert(false);
      }
      case jp.Error(msg,_)    => {
        println("ERROR: " + msg)
        assert(false);
      }
      case _                  => assert(false)
    }
  }

  lazy val notaryAST = parse(getClass.getResource("/examples/notary.js"))
  "notary.js" should "parse" in {
    notaryAST match {
      case jp.Success(ast, _) => ast
      case jp.Failure(msg,_)  => {
        println("FAILURE: " + msg)
        assert(false);
      }
      case jp.Error(msg,_)    => {
        println("ERROR: " + msg)
        assert(false);
      }
      case _                  => assert(false)
    }
  }
}
