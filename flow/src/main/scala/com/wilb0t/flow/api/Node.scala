package com.wilb0t.flow.api

import com.weiglewilczek.slf4s.Logging

abstract class ExitPort {
  def name: String
  def description: String
}

trait Executable {
  def name: String
  def execute: ExitPort
}

abstract class FlowNode extends Executable {
  def nextNode(exitPort: ExitPort): Option[FlowNode]
}

class Flow(val head: FlowNode) extends Logging {
  def execute: List[(FlowNode, ExitPort)] = {
    def execNode(node: Option[FlowNode], path: List[(FlowNode, ExitPort)]): List[(FlowNode, ExitPort)] = {
      node match {
        case None => 
          logger.info("Flow finished")
          path.reverse
        case Some(n) =>
          logger.info("Executing node: "+n.name)
          val exitPort = n.execute
          logger.info("Got exit port: "+exitPort.name+" - "+exitPort.description)
          val nextNode = n.nextNode(exitPort)
          execNode(nextNode, (n, exitPort) :: path)
      }

    }
     
    execNode(Some(head), Nil)
  }

}

