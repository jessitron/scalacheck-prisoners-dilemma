package prisoners_dilemma

import akka.actor._

object FreeForAll {
  type Matchup = (Player, Player)
  type AllTheScores = Map[Matchup, ScoreSet]
  case object GiveMeTheScore

  case class UpdatedScore(matchup: Matchup, score: ScoreSet)

}

import FreeForAll._

class MatchupActor(matchup: Matchup, rules: Rules, scorekeeper: ActorRef) extends Actor {

  val rounds: Stream[ScoreSet] = RoundStrategy.moves(matchup._1.strategy.newGame(), matchup._2.strategy.newGame()) map (Rules.score(rules,_))

  override def preStart() {
    self ! rounds
  }

  var scoresSoFar = (0,0)
  def scoreRound(scoreSet: ScoreSet) {
    scoresSoFar = (scoresSoFar._1 + scoreSet._1, scoresSoFar._2 + scoreSet._2)
  }

  def receive = {
    case ((score:ScoreSet) #:: moreRounds) =>
      scoreRound(score);
      scorekeeper ! UpdatedScore(matchup, scoresSoFar)
      self ! moreRounds
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

   def receive = {
     case GiveMeTheScore => sender ! scores
     case UpdatedScore(m: Matchup, s: ScoreSet) => scores = scores + (m -> s)
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
