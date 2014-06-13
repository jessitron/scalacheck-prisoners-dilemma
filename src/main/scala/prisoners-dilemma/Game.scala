package prisoners_dilemma

import scala.concurrent.duration._
import akka.actor._

case class Rules(temptationToDefect: Points,
                 rewardForMutualCooperation: Points,
                 punishmentForMutualDefection: Points,
                 suckersPenalty: Points)


object Rules {
  def score(rules: Rules, moves: MoveSet): ScoreSet =
    moves match {
      case (Defect, Cooperate) =>
        (rules.temptationToDefect, rules.suckersPenalty)
      case (Cooperate, Defect) =>
        (rules.suckersPenalty, rules.temptationToDefect)
      case (Cooperate, Cooperate) =>
        (rules.rewardForMutualCooperation, rules.rewardForMutualCooperation)
      case (Defect, Defect) =>
        (rules.punishmentForMutualDefection, rules.punishmentForMutualDefection)
    }
}

case class Player(name: String, strategy: Strategy)

case class Outcome(player: Player, opponent: Player, score: Points, myMoves: Seq[Move], opponentMoves:Seq[Move])

case class AggregateOutcome(player: Player, score: Points)

object Game {

  def oneOnOne(rules:Rules, turns: Int)(p1: Player, p2:Player): (Outcome, Outcome) = {
    val moves = Strategy.moves(p1.strategy, p2.strategy).take(turns)
    val scores = moves map (Rules.score(rules,_))

    val p1score = scores.map(_._1).sum
    val p2score = scores.map(_._2).sum

    val p1moves = moves.map(_._1)
    val p2moves = moves.map(_._2)
    val p1Outcome = Outcome(p1, p2, p1score, p1moves, p2moves)
    val p2Outcome = Outcome(p2, p1, p2score, p2moves, p1moves)
    (p1Outcome, p2Outcome)
  }

  import scala.concurrent._
  import FreeForAll._
  def eachOnEach(rules: Rules)(system: ActorSystem,
    players: Seq[Player], timeLimit: FiniteDuration): Seq[AggregateOutcome] = {
      val game = system.actorOf(Props(new EachOnEach(players, rules)))
      Thread.sleep(timeLimit.toMillis); // should be some less? Also shouldn't hold a thread
      import akka.pattern.ask
      val akkaTimeout = akka.util.Timeout(timeLimit)
      val results = Await.result(
        game.ask(GiveMeTheScore)(akkaTimeout).mapTo[AllTheScores], timeLimit)
      game ! PoisonPill

      // I should use monoids but I don't feel like bringing in scalaz
      // this is TERRIBLE functional style
      results.flatMap { case ((p1,p2),(s1, s2)) => Seq((p1,s1),(p2,s2)) }.
      groupBy(_._1).map { case (p, pandscores) => (p, pandscores.map(_._2).sum)}.
      map { case (p, score) => AggregateOutcome(p, score)}.toSeq
  }
}
