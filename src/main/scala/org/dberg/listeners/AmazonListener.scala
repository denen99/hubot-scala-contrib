package org.dberg.listeners

import com.iheart.listeners.AmazonHelpers
import org.dberg.hubot.HubotBase
import org.dberg.hubot.listeners.Listener.{CallbackFailure, CallbackSuccess}
import org.dberg.hubot.listeners.{Listener, ListenerType}
import org.dberg.hubot.models.Message
import scala.util.control.NonFatal


class EC2Listener(hubot: HubotBase)  extends Listener(hubot, "\\s*ec2\\s+query\\s+([^ ]+)\\s+([^ ]+)\\s*(.*)", ListenerType.Hear) with AmazonHelpers {

  val helpString = Some("ec2 query <dev | staging | prod> <regex> <tag1,tag2> -> Query EC2 instances by environment and one or more tags")
  def runCallback(message: Message, groups: List[String]) = {
    val env = groups.head
    val regex = groups(1)
    val tags: List[String] = groups(2).split(",").toList

    def tidyRegex(r: String) = r.contains("*") match {
      case true => r
      case false => "*" + r + "*"
    }

    try {
      val instances = getInstances(tidyRegex(regex),tags,env).mkString("\n")
      val msg = Message(message.user,instances,message.messageType)
      robot.send(msg)
      CallbackSuccess
    } catch { case NonFatal(e) =>
      robot.send(Message(message.user, "Error querying EC2", message.messageType))
      CallbackFailure(e)
    }

  }
}


class ElbListener(hubot: HubotBase) extends Listener(hubot, "\\s*elb\\s+([^ ]+)\\s+([^ ]+)\\s+(.+)", ListenerType.Hear) with AmazonHelpers {

  val helpString = Some("elb  <query | info> <dev | staging | prod > <regex | elbname> -> query ELBs or get info for a specific ELB")

  def runCallback(message: Message, groups: List[String]) = {
    val cmd = groups.head
    val env = groups(1)
    val str = groups(2)

    cmd match {
      case "query" =>
        try {
          queryELbs(str, env).foreach { elb =>
            robot.send(Message(message.user, s"${elb._1} -> ${elb._2}", message.messageType))
          }
          CallbackSuccess
        } catch { case NonFatal(e) =>
          robot.send(Message(message.user,"Error querying for ELBs", message.messageType))
          CallbackFailure(e)
        }
      case "info" =>
        try {
          getElbInfo(str, env).foreach { x =>
            robot.send(Message(message.user, "Instances: " + x.getOrElse("instances", "No instances"), message.messageType))
            robot.send(Message(message.user, "Instance Port: " + x.getOrElse("ports", "No ports"), message.messageType))
            robot.send(Message(message.user, "HealthCheck: " + x.getOrElse("healthcheck", "No Healthcheck"), message.messageType))
          }
          CallbackSuccess
        } catch {
          case NonFatal(e) =>
            robot.send(Message(message.user,"Error querying for ELBs", message.messageType))
            CallbackFailure(e)
        }
      case x =>
        robot.send(Message(message.user,"Sorry unknown command " + x + " for elb",message.messageType))
        CallbackSuccess
    }
  }
}

class Route53Listener(hubot: HubotBase) extends Listener(hubot, "\\s*route53\\s+query\\s+([^ ]+)\\s+(.+)\\s*",ListenerType.Hear) with AmazonHelpers {

  val helpString = Some("route53 query <prod | nonprod> [regex]  -> Pick one environment and a string to match each DNS record against")

  private def validEnv(env: String) = List("prod","nonprod").contains(env)

  def runCallback(message: Message, groups: List[String]) = {
    val env = groups.head
    val regex = groups(1)

    if (validEnv(env)) {
      try {
        val records = fetchRecords(regex, env)
        records.foreach { rec =>
          robot.send(Message(message.user, s"${rec._1} ${rec._2} ${rec._3}", message.messageType))
        }
        CallbackSuccess
      } catch {
        case NonFatal(e) =>
          robot.send(Message(message.user,"Error querying for Route53 Records", message.messageType))
          CallbackFailure(e)
      }
    }
    else {
      robot.send(Message(message.user,"Sorry " + env + " is not a valid env, only prod and nonprod",message.messageType))
      CallbackSuccess
    }

  }

}
