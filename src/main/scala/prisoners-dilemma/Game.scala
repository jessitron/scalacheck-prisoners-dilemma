package prisoners_dilemma

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
    val p1Outcome = Outcome(p1, p2, 0, Seq(), Seq())
    val p2Outcome = Outcome(p2, p1, 0, Seq(), Seq())
    (p1Outcome, p2Outcome)
  }

}
