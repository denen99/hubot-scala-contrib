package org.dberg.listeners

import org.dberg.hubot.HubotBase
import org.dberg.hubot.listeners.Listener.CallbackSuccess
import org.dberg.hubot.listeners.{Listener, ListenerType}
import org.dberg.hubot.models.{Message, MessageType, User}


//Blasts a message to a specific set of rooms
class BlastListener(hubot: HubotBase) extends Listener(hubot, "\\s*blast\\s+([^ ]+)\\s*(.*)", ListenerType.Hear) {

  val helpString = Some("blast <msg|add|del|list> [ msg | room ] -> Manage blast notifications to various rooms")

  def runCallback(message: Message, groups: List[String]) = {
    val cmd = groups.head
    lazy val param = groups(1)
    val brainKey = "blast"

    def getCurrent = brain.get[List[String]](brainKey).getOrElse(List())

    cmd match {
      case "add" =>
        val current = getCurrent
        brain.set[List[String]](brainKey,current :+ param)
        robot.send(Message(message.user,"Added room" + param,message.messageType))
        CallbackSuccess
      case "del" =>
        val current = getCurrent
        brain.set[List[String]](brainKey, current.filter(c => c != param))
        CallbackSuccess
      case "msg" =>
        val current = getCurrent
        current.foreach { room =>
          robot.send(Message(User(room),param,MessageType.GroupMessage))
        }
        CallbackSuccess
      case "list" =>
        val current = getCurrent
        current.foreach { room => robot.send(Message(message.user,room,message.messageType))}
        CallbackSuccess
      case x =>
        robot.send(Message(message.user,"Sorry, cant use blast with command " + x,message.messageType))
        CallbackSuccess
    }
  }
}