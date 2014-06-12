package prisoners_dilemma

import org.scalacheck._

object GameGen {
  import Gen._
  import StrategyGen._

  val playerGen =  for {
    name <- Gen.alphaStr
    strategy <- strategyGen
  } yield Player(name, strategy)

  implicit val arbPlayer = Arbitrary(playerGen)
}

object GameTest extends Properties("An iterated game of Prisoners Dilemma") {

  import GameGen._
  import RuleGenerators._
  import Prop._

  property("The results are fairly reported") = forAll(playerGen, playerGen, ruleGen(100), Gen.posNum[Int]) {
    (p1: Player, p2: Player, rules: Rules, turns: Int) =>

      val (outcome1, outcome2) = Game.oneOnOne(rules, turns)(p1, p2)

      (outcome1.myMoves == outcome2.opponentMoves && outcome2.myMoves == outcome2.opponentMoves)
      (outcome1.player == p1 && outcome2.player == p2) &&
      (outcome1.opponent == p2 && outcome2.opponent == p1)

  }

  property("The results are consistent with the rules") = forAll {
    (p1: Player, p2: Player, rules: Rules, turns: Int) =>

      val (outcome1, outcome2) = Game.oneOnOne(rules, turns)(p1, p2)

      val minPossibleScore = rules.suckersPenalty * turns
      val maxPossibleScore = rules.temptationToDefect * turns
      val maxGlobalScore = rules.rewardForMutualCooperation * turns * 2

      val allOutcomes = Seq(outcome1, outcome2)

      val maxDefections = allOutcomes.maxBy(_.myMoves.count(_ == Defect))
      val maxScore = allOutcomes.maxBy(_.score)

      val totalScore = allOutcomes.map(_.score).sum


      Prop.all(Seq(outcome1, outcome2).map(_.score).map(score =>
            (score <= maxPossibleScore && score >= minPossibleScore) :| s"Score of $score is impossible"
          ): _*) &&
      (((totalScore) <= maxGlobalScore) :| s"Total score of ${totalScore} is impossible") &&
      (maxDefections == maxScore) :| "Wait, how did you score higher without defecting?"



  }

}
