package bpmn2rho

import js2rho.{RhoBuilder, Name, Process, Printer, RhoPrinter}

import scala.xml.XML
import scala.xml.Node

object PizzaOrderTest {
  def main(args: Array[String]) = {
    println("Hello, world!")
    // https://github.com/bpmn-io/bpmn-js-examples/blob/master/bundling/resources/pizza-collaboration.bpmn
    val pizzaModelRes = getClass.getResource("/pizza-collaboration.bpmn")

    println(pizzaModelRes)

    val doc = XML.load(pizzaModelRes)
    // println(doc \\ "task")
    println(doc \\ "startEvent")

    val printer = Printer.fromPrintStream(System.out)
    val bld = new RhoPrinter()

    toRho(doc, bld)._printOn(printer)

  }

  // TODO: monadic style instead of Builder?
  def toRho(bpml: Node, bld: RhoBuilder): Process = {
    val nil = bld.Nil()
    def par(ps: Seq[Process]): Process = ps match {
      case Seq()         => nil
      case Seq(first)    => first
      case first :: rest => bld.Par(first, par(rest))
    }
    // xmlns:semantic="http://www.omg.org/spec/BPMN/20100524/MODEL"
    val starts =
      for (startEvent <- bpml \\ "startEvent";
           id = bld.Var((startEvent \ "@id") text);
           name = bld.primitive((startEvent \ "@name") text))
        yield bld.receiving(List((List(bld.Quote(name)), id)), nil)
    par(starts)
  }
}
