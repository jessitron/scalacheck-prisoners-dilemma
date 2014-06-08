package prisoners_dilemma

import org.scalacheck._

object RuleGenerators {

  def ruleGen(max: Points): Gen[Rules] = for {
     t <- Gen.choose(3, max)
     r <- Gen.choose(2, (t - 1))
     p <- Gen.choose(1, (r - 1))
     s <- Gen.choose(0, (p - 1))
     if (2 * r > (t + s))
  } yield Rules(temptationToDefect = t,
                rewardForMutualCooperation = r,
                punishmentForMutualDefection = p,
                suckersPenalty = s)

  implicit val arbRules: Arbitrary[Rules] = Arbitrary(ruleGen(100))

  val move: Gen[Move] = Gen.oneOf(Cooperate, Defect)
  implicit val arbMoves: Arbitrary[Move] = Arbitrary(move)

}

object GameTest extends Properties("Prisoners Dilemma") {
  import Prop._
  import Gen._
  import RuleGenerators._
  property("Defection is always better for me") =
     forAll {(rules: Rules, theirMove: Move) =>
       val ifIDefect = Rules.score(rules, (Defect, theirMove))._1
       val ifICooperate = Rules.score(rules, (Cooperate, theirMove))._1

       ifIDefect > ifICooperate
     }

  property("The best possible global outcome is both cooperate") =
    forAll{ (rules: Rules, moves: MoveSet) =>
      (moves != (Cooperate, Cooperate)) ==> {
        val bestResult = Rules.score(rules, (Cooperate, Cooperate))
        val myResult = Rules.score(rules, moves)
        total(bestResult) > total(myResult)
      }
    }

  def total(scores: ScoreSet) = scores._1 + scores._2

  property("The game is fair") =
    forAll {(rules: Rules, moves: MoveSet) =>
      val oneWay = Rules.score(rules, moves)
      val theOtherWay = Rules.score(rules, moves.swap)

      oneWay.swap == theOtherWay
    }

}
