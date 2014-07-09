package prisoners_dilemma

import akka.actor.ActorContext
import java.net.URI

class RemoteStrategy(context:ActorContext, firstMove: URI)
extends Strategizer {

  def newGame(): RoundStrategy = {
     // call first URI
     val (move, uri) = parseResponse("la la ala la ala")
     new RemoteRoundStrategy(context, move, uri)
  }

  private def parseResponse(response: String /* what am I */): (Move, URI) = ???

 private class RemoteRoundStrategy(context:ActorContext, myMove: Move, nextURI: URI) extends RoundStrategy {
  val currentMove = myMove
  def next(m: Move) = {
    // call URI
    val (nextMove, laterURI) = parseResponse("bananas")
    new RemoteRoundStrategy(context, nextMove, laterURI)
  }
 }

}
