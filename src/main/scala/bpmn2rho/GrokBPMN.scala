package bpmn2rho

import scala.xml.Node

import js2rho.{RhoBuilder, Name, Process, Printer, RhoPrinter}

object GrokBPMN {
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
        yield bld.receiving(
          List((List(bld.Var("_")), id)),
          bld.send(bld.Var("console"), Seq(name))
        )
    bld.new_(List(("console", Some("rho:io:stdout"))), par(starts))
  }
}
