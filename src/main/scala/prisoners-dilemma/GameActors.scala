package prisoners_dilemma

import akka.actor._
import akka.event.LoggingReceive

object FreeForAll {
  type Matchup = (Player, Player)
  type AllTheScores = Map[Matchup, ScoreSet]
  case object GiveMeTheScore

  case class UpdatedScore(matchup: Matchup, score: ScoreSet)

}

import FreeForAll._

class MatchupActor(matchup: Matchup, rules: Rules, scorekeeper: ActorRef) extends Actor {

  val (oneBird, twoBird) = matchup

  val rounds: Stream[ScoreSet] =
    RoundStrategy.moves(oneBird.strategy.newGame(), twoBird.strategy.newGame()).
      map(Rules.score(rules,_))

  override def preStart() {
    self ! rounds
  }

  var scoresSoFar = (0,0)
  def scoreRound(scoreSet: ScoreSet) {
    scoresSoFar = add(scoresSoFar, scoreSet)
  }
  def add(scores: ScoreSet, moreScores: ScoreSet) = {
    val (a1, a2) = scores
    val (b1, b2) = moreScores
    (a1 + b1, a2 + b2)
  }

  def receive = {
    case (s: Stream[ScoreSet]) =>
      val score = s.head
      println(s"Scorinating $s")
      scoreRound(score)
      scorekeeper ! UpdatedScore(matchup, scoresSoFar)
      self ! s.tail
  }

}

class EachOnEach(players: Seq[Player], rules: Rules) extends Actor {

  override def preStart() {
    val matchupSeqs = players.sliding(2,1).toList :+ List(players.head, players.last)
    val matchups :Seq[Matchup]= matchupSeqs.map( s => (s(0), s(1)))

    matchups.foreach { m =>
      context.actorOf(Props(classOf[MatchupActor], m, rules, self))
    }
  }

   var scores: AllTheScores = Map[Matchup, ScoreSet]()

   var count = 0
   def receive = LoggingReceive {
     case GiveMeTheScore => sender ! scores; println("scores req")
     case UpdatedScore(m: Matchup, s: ScoreSet) => scores = scores + (m -> s); count = count+1; if(count%5000 ==0) println(rules+" "+count)
     case x => println(s"What is $x?")
   }
}

// this shouldn't be here but I want it for testing
case class SendThis(x: Any)
class CheckIn(on: ActorRef) extends Actor {
  def receive = {
    case SendThis(x: Any) => on ! x
    case other => println(s"Received: $other")
  }
}
