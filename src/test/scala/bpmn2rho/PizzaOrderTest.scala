package bpmn2rho

import js2rho.{RhoBuilder, Name, Process, Printer, RhoPrinter}

import scala.xml.XML
import scala.xml.Node

import org.scalatest.FlatSpec

class PizzaOrderTest extends FlatSpec {
  // https://github.com/bpmn-io/bpmn-js-examples/blob/master/bundling/resources/pizza-collaboration.bpmn
  val pizzaModelRes = getClass.getResource("/pizza-collaboration.bpmn")
  lazy val pizzaDoc = XML.load(pizzaModelRes)

  "PizzaDoc" should "be XML" in {
    pizzaDoc match {
      case x: Node => x
      case _       => assert(false)
    }
  }

  lazy val pizzaProc = GrokBPMN.toRho(pizzaDoc, new RhoPrinter())
  "PizzaDoc" should "compile to Rholang process" in {
     pizzaProc match {
      case x: Process => null
      case _ => assert(false)
    }
  }

  "PizzaProc" should "print nicely" in {
    val buf = new StringBuilder
    val p = Printer.inMemory(buf)
    pizzaProc._printOn(p)
    val code = buf.toString()
    println(code)
    assert(code.contains("console!(\"Order received\")"))
    assert(code.contains("console!(\"Hungry for pizza\")"))
  }
  
  def main(args: Array[String]) = {
    println(pizzaModelRes)

    val doc = XML.load(pizzaModelRes)
    // println(doc \\ "task")
    println(doc \\ "startEvent")

    val printer = Printer.fromPrintStream(System.out)
    val bld = new RhoPrinter()

    GrokBPMN.toRho(doc, bld)._printOn(printer)
  }

}
