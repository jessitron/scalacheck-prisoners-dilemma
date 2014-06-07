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
        (rules.suckersPenalty, 0)//rules.temptationToDefect)
      case (Cooperate, Cooperate) =>
        (rules.rewardForMutualCooperation, rules.rewardForMutualCooperation)
      case (Defect, Defect) =>
        (rules.punishmentForMutualDefection, rules.punishmentForMutualDefection)
    }
}
