package com.wilb0t.flow.api

import org.scalatest.FunSuite
import com.weiglewilczek.slf4s.Logging

case class PassExit() extends ExitPort {
  val description = "Node Passed"
}

case class FailExit() extends ExitPort {
  val description = "Node Failed"
}

class PassAction extends Action with Logging {
  override def execute: ExitPort = {
    logger.info("Emitting PassExit")

    PassExit()
  }
}

class FailAction extends Action with Logging {
  override def execute: ExitPort = {
    logger.info("Emitting FailExit")

    FailExit()
  }
}

class FlowTest extends FunSuite with Logging {

  test("simple flow") {

    val flow = 
      new Flow("TestFlow", List[FlowNode](
            new FlowNode("HeadNode", new PassAction(), Map[ExitPort, String](
              ( PassExit() -> "MidNode" ),
              ( FailExit() -> "EndNode" )
            )),
            new FlowNode("MidNode", new PassAction(), Map[ExitPort, String](
              ( PassExit() -> "EndNode" ),
              ( FailExit() -> "EndNode" )
            )),
            new EndFlowNode("EndNode", new FailAction())
            )
          )


    val path = flow.execute
    logger.info("Got path: "+path)
  }
}

