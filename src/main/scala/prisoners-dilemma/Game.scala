package prisoners_dilemma

import scala.concurrent.duration._

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

  def eachOnEach(rules: Rules)(players: Seq[Player], timeLimit: Duration): Seq[Outcome] = Seq()
}
