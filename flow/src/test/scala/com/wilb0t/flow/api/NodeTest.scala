package com.wilb0t.flow.api

import org.scalatest.FunSuite
import com.weiglewilczek.slf4s.Logging

case class PassExit() extends ExitPort {
  val name = "PassExit"
  val description = "Node Passed"
}

case class FailExit() extends ExitPort {
  val name = "FailExit"
  val description = "Node Failed"
}

class FailNode(val name: String) extends Executable with Logging {
  override def execute: ExitPort = {
    logger.info(name+" Executing")

    FailExit()
  }

  override def toString: String = {
    this.getClass().getSimpleName()+":"+name
  }
}

class PassNode(val name: String) extends FlowNode with Logging {
  override def execute: ExitPort = {
    logger.info(name+" Executing")

    PassExit()
  }

  override def nextNode(exitPort: ExitPort): Option[FlowNode] = {
    exitPort match {
      case PassExit() => new Some(new FailNode("FailNode1"))
      case _ => None
    }
  }

  override def toString: String = {
    this.getClass().getSimpleName()+":"+name
  }
}

class FlowTest extends FunSuite with Logging {
  test("simple flow") {
    val head = new PassNode("PassNode1")
    val flow = new Flow(head)

    val path = flow.execute
    logger.info("Got path: "+path)
  }
}

